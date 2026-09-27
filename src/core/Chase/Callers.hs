{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Who calls what, across a whole project.
--
-- ============================================================================
-- THIS IS THE COVERAGE PASS, POINTED SOMEWHERE ELSE
-- ============================================================================
--
-- Chase.Coverage parses a file, takes every top-level binding, collects the
-- variable references in its body, and inverts the result into a map from
-- referenced name to the bindings that mention it. Run over test roots,
-- that is test coverage. Run over SOURCE directories, the same index is a
-- call graph, and a call graph is exactly what the `consumes` field of an
-- invariant records.
--
-- %decision DriftCheckerScopeIsLocal says consumes entries are not
-- validated because doing so "would require parsing the consumer module
-- too", which was true of the drift checker in isolation and untrue of the
-- pipeline, which parses every module anyway.
--
-- ============================================================================
-- WHAT GETS SCANNED: THE PROJECT'S OWN hs-source-dirs
-- ============================================================================
--
-- The project says which directories hold its source, in the
-- hs-source-dirs fields of its .cabal file, and this reads them rather
-- than walking the tree.
--
-- Walking the tree was tried first and failed twice on pelotero-engine.
-- It reached script/concat_archive/, which holds catsrc dumps (many
-- modules concatenated into one .hs file, not valid Haskell), and it
-- reached .direnv/flake-inputs/, which holds the full source of every
-- flake input including Cabal and haskell-language-server. Each file
-- failed to parse and printed a warning, and the run became a wall of
-- rejections burying the result. Reading hs-source-dirs is not a
-- heuristic; it is the build's own definition of the source tree, and
-- neither of those directories is in it.
--
-- Only library, executable and common stanzas are read. Test suites and
-- benchmarks are left out on purpose: a test calling a function is
-- coverage, which chase already reports on its own ? line, and folding
-- test callers into consumes would bury the real consumers under the
-- tests that exercise them.
--
-- If no .cabal file is found, or it declares no source dirs, the fallback
-- is the source file's own directory, and the caller is told which root
-- was used so a small index is never silent.
--
-- ============================================================================
-- INHERITED LIMITS
-- ============================================================================
--
-- Everything Chase.Coverage's walk cannot see, this cannot see either, and
-- those limits are written down in %open_issue UnqualifiedNameMatching and
-- %open_issue HigherOrderReferencesMissed: names match unqualified, so two
-- modules exporting the same name share callers; references introduced by
-- Template Haskell splices, by instance dispatch, or through a record
-- selector are invisible.
module Chase.Callers
  ( CallerIndex
  , ScanPlan (..)
  , planScan
  , buildCallerIndex
  , callersOf
  , callerNames
  , renderCallers
  , sourceDirsFromCabal
  ) where

import           Control.Monad    (forM)
import           Data.Char        (isSpace)
import qualified Data.List
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (catMaybes)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           System.Directory
  ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath
  ( (</>), takeDirectory, takeExtension )

import qualified Chase.Coverage   as Coverage
import           Chase.Types

-- | Referenced name to the bindings that reference it. Identical in shape
-- to Coverage.TestRefIndex because it is the same index built from
-- different files.
type CallerIndex = Map.Map Text [TestRef]

-- | Where a scan will look, and how that was decided, so the caller can
-- print one honest line about it.
data ScanPlan = ScanPlan
  { spRoots  :: [FilePath]
  , spSource :: Text
  -- ^ e.g. "hs-source-dirs from /path/foo.cabal", or the fallback reason
  }
  deriving stock (Show)

-- | Decide what to scan for a given source file.
--
-- Walks up from the file to the nearest .cabal and reads its
-- hs-source-dirs. Falls back to the file's own directory when there is no
-- .cabal or it declares nothing, and says so.
planScan :: FilePath -> IO ScanPlan
planScan start = do
  found <- findCabal (takeDirectory start) (40 :: Int)
  case found of
    Nothing ->
      pure
        ScanPlan
          { spRoots = [takeDirectory start]
          , spSource = "no .cabal file above the source; scanning its own directory only"
          }
    Just cabalPath -> do
      contents <- TIO.readFile cabalPath
      let base = takeDirectory cabalPath
          dirs = sourceDirsFromCabal contents
      existing <- fmap catMaybes $ forM dirs \d -> do
        let full = base </> T.unpack d
        ok <- doesDirectoryExist full
        pure (if ok then Just full else Nothing)
      if null existing
        then
          pure
            ScanPlan
              { spRoots = [takeDirectory start]
              , spSource = T.pack cabalPath <> " declares no existing hs-source-dirs; scanning the source file's directory only"
              }
        else
          pure
            ScanPlan
              { spRoots = existing
              , spSource = "hs-source-dirs from " <> T.pack cabalPath
              }
  where
    findCabal _ 0 = pure Nothing
    findCabal dir n = do
      ok <- doesDirectoryExist dir
      entries <- if ok then listDirectory dir else pure []
      case [e | e <- entries, takeExtension e == ".cabal"] of
        (c : _) -> pure (Just (dir </> c))
        [] ->
          let up = takeDirectory dir
           in if up == dir then pure Nothing else findCabal up (n - 1 :: Int)

-- | The hs-source-dirs of every library, executable and common stanza.
--
-- A deliberately small reader, not a cabal parser: it tracks which
-- top-level stanza it is in by lines starting in column zero, reads the
-- hs-source-dirs field, and follows indented continuation lines. Commas
-- and whitespace both separate entries. Test suites, benchmarks, flags and
-- source-repository stanzas are skipped.
--
-- Checked against chase's own .cabal: it returns the four library
-- directories and the two executable directories, and nothing from the
-- test or benchmark stanzas.
sourceDirsFromCabal :: Text -> [Text]
sourceDirsFromCabal = Data.List.nub . go False . T.lines
  where
    go _ [] = []
    go inSrc (l : ls)
      | startsStanza l = go (wanted l) ls
      | not inSrc = go inSrc ls
      | Just v <- field "hs-source-dirs" l =
          let (cont, rest) = span isContinuation ls
           in entries (T.unwords (v : cont)) <> go inSrc rest
      | otherwise = go inSrc ls

    startsStanza l =
      not (T.null l)
        && not (isSpace (T.head l))
        && any (`T.isPrefixOf` T.toLower l) stanzaWords

    stanzaWords =
      [ "library", "executable", "test-suite", "benchmark", "common"
      , "foreign-library", "flag", "source-repository"
      ]

    wanted l = any (`T.isPrefixOf` T.toLower l) ["library", "executable", "common"]

    field name l =
      let t = T.stripStart l
       in if T.toLower name `T.isPrefixOf` T.toLower t
            then Just (T.drop 1 (T.dropWhile (/= ':') t))
            else Nothing

    isContinuation l =
      not (T.null l)
        && isSpace (T.head l)
        && not (T.any (== ':') l)
        && not (T.all isSpace l)

    entries = filter (not . T.null) . T.words . T.map (\c -> if c == ',' then ' ' else c)

-- | Parse every .hs and .purs file under the planned roots and invert
-- their references into a call graph.
--
-- Parse failures are counted and summarised in one line rather than
-- printed one by one; the full list is printed only when verbose. A file
-- that fails contributes nothing, so a failure costs completeness, never
-- correctness.
buildCallerIndex :: Bool -> ScanPlan -> IO CallerIndex
buildCallerIndex verbose ScanPlan{..} = do
  files <- concat <$> mapM scanFiles spRoots
  results <- forM files \p -> do
    r <- Coverage.parseTestFile p
    pure (p, r)
  let failures = [(p, e) | (p, Left e) <- results]
      bindings = concat [bs | (_, Right bs) <- results]
      idx = Coverage.buildTestIndex bindings
  TIO.putStrLn ("callers: " <> spSource)
  putStrLn
    ( "callers: "
        <> show (length files - length failures)
        <> " of "
        <> show (length files)
        <> " files parsed, "
        <> show (Map.size idx)
        <> " names referenced"
    )
  if verbose && not (null failures)
    then mapM_ (\(p, ParseFailure{..}) -> putStrLn ("  could not parse " <> p) >> TIO.putStrLn ("    " <> T.take 120 pfMsg)) failures
    else pure ()
  pure idx

-- | The callers of one name.
--
-- Same-module callers are KEPT. "wraps runSession" is one of the most
-- useful facts about runTransaction and it is a same-module edge; the
-- only edge worth dropping is a function's reference to itself, which
-- Coverage.buildTestIndex has already removed.
callersOf :: CallerIndex -> Text -> [TestRef]
callersOf idx name = Map.findWithDefault [] name idx

-- | Callers of a name as "Module.function", deduplicated, excluding the
-- name's own definition site. This is what fills an invariant's consumes
-- field.
callerNames :: CallerIndex -> Text -> Text -> [Text]
callerNames idx modName name =
  Data.List.nub
    [ trModule r <> "." <> trFunction r
    | r <- callersOf idx name
    , not (trModule r == modName && trFunction r == name)
    ]

-- | The caller lines for one module's signatures, for the prompt.
--
-- A name with no callers gets an explicit line rather than silence, for
-- the same reason coverage prints "no test references": an exported
-- function nothing calls is the actionable case, and hiding it would hide
-- the most interesting thing on the page.
renderCallers :: CallerIndex -> Text -> [Text] -> Text
renderCallers idx modName names =
  T.unlines
    ( ("%callers " <> modName)
        : map one names
    )
  where
    one n =
      case callerNames idx modName n of
        [] -> "  " <> n <> ": no callers found in the scanned source dirs"
        cs -> "  " <> n <> ": " <> T.intercalate ", " cs

-- | Every .hs and .purs file under a root.
scanFiles :: FilePath -> IO [FilePath]
scanFiles root = do
  isFile <- doesFileExist root
  if isFile
    then pure [root | takeExtension root `elem` [".hs", ".purs"]]
    else do
      exists <- doesDirectoryExist root
      if not exists
        then pure []
        else do
          entries <- Data.List.sort <$> listDirectory root
          results <- forM (map (root </>) entries) \p -> do
            isDir <- doesDirectoryExist p
            if isDir
              then scanFiles p
              else pure [p | takeExtension p `elem` [".hs", ".purs"]]
          pure (concat results)