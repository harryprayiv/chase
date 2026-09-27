{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BlockArguments    #-}

module Chase.Pipeline
  ( ChaseConfig (..)
  , defaultConfig
  , runChase
  , attachAnnotations
  , checkAnnotationDrift
  , annotationScore
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha, isAlphaNum, isDigit, isUpper)
import Control.Monad (forM, forM_, when)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory )
import System.FilePath
  ( (</>), (<.>), takeDirectory, takeExtension, takeFileName
  , dropExtension, makeRelative, equalFilePath
  )
import qualified Data.List

import           Chase.Types
import           Chase.Parse
import           Chase.Render
import qualified Chase.Coverage as Coverage

data ChaseConfig = ChaseConfig
  { cfgSourceRoots :: [FilePath]
  , cfgOutputDir   :: FilePath
  , cfgBundleFile  :: Maybe FilePath
  , cfgAnnotations :: Map Text ModuleAnnotations
  , cfgTestRoots   :: [FilePath]
  , cfgVerbose     :: Bool
  }

defaultConfig :: ChaseConfig
defaultConfig = ChaseConfig
  { cfgSourceRoots = []
  , cfgOutputDir   = "chase"
  , cfgBundleFile  = Nothing
  , cfgAnnotations = Map.empty
  , cfgTestRoots   = []
  , cfgVerbose     = False
  }

runChase :: ChaseConfig -> IO ()
runChase cfg = do
  testIdx <- buildTestIndexOrEmpty cfg
  case cfgBundleFile cfg of
    Nothing     -> runPerFile  cfg testIdx
    Just bundle -> runBundled  cfg bundle testIdx

-- | Parse every .hs and .purs file under cfgTestRoots and build the
-- global reference index. If cfgTestRoots is empty, returns an empty
-- map and the rest of the pipeline emits no ? lines.
buildTestIndexOrEmpty :: ChaseConfig -> IO Coverage.TestRefIndex
buildTestIndexOrEmpty ChaseConfig{..}
  | null cfgTestRoots = pure Map.empty
  | otherwise = do
      when cfgVerbose $ do
        putStrLn ""
        putStrLn $ "test roots: "
                <> Data.List.intercalate ", " cfgTestRoots
      files <- concat <$> mapM findSourceFiles cfgTestRoots
      let testFiles = filter (\p -> takeExtension p `elem` [".hs", ".purs"]) files
      bindings <- forM testFiles \p -> do
        when cfgVerbose $ putStrLn $ "scan   " <> p
        result <- Coverage.parseTestFile p
        case result of
          Left ParseFailure{..} -> do
            putStrLn $ "WARN: test parse failed for " <> pfPath
            TIO.putStrLn $ "  " <> pfMsg
            pure []
          Right bs -> pure bs
      let idx = Coverage.buildTestIndex (concat bindings)
      when cfgVerbose $ do
        putStrLn $ "index  " <> show (Map.size idx)
                <> " distinct names referenced by tests"
        putStrLn ""
      pure idx

runPerFile :: ChaseConfig -> Coverage.TestRefIndex -> IO ()
runPerFile ChaseConfig{..} testIdx = do
  createDirectoryIfMissing True cfgOutputDir
  files <- concat <$> mapM findSourceFiles cfgSourceRoots
  forM_ files \src -> do
    when cfgVerbose $ putStrLn $ "parse  " <> src
    result <- parseSourceFile src
    case result of
      Left ParseFailure{..} -> do
        putStrLn $ "WARN: parse failed for " <> pfPath
        TIO.putStrLn $ "  " <> pfMsg
      Right structural -> do
        let merged   = applyPasses cfgAnnotations cfgTestRoots testIdx structural
            outPath  = mkOutputPath cfgOutputDir cfgSourceRoots src
            rendered = renderChaseFile merged
            drift    = checkAnnotationDrift merged
        forM_ drift \w ->
          putStrLn $ "  drift: " <> T.unpack w
        createDirectoryIfMissing True (takeDirectory outPath)
        TIO.writeFile outPath rendered
        when cfgVerbose $ putStrLn $ "write  " <> outPath

runBundled :: ChaseConfig -> FilePath -> Coverage.TestRefIndex -> IO ()
runBundled ChaseConfig{..} bundlePath testIdx = do
  createDirectoryIfMissing True (takeDirectory bundlePath)
  files <- concat <$> mapM findSourceFiles cfgSourceRoots
  failuresRef  <- newIORef (0 :: Int)
  successesRef <- newIORef (0 :: Int)
  blocks       <- forM files \src -> do
    when cfgVerbose $ putStrLn $ "parse  " <> src
    result <- parseSourceFile src
    case result of
      Left ParseFailure{..} -> do
        modifyIORef' failuresRef (+ 1)
        putStrLn $ "WARN: parse failed for " <> pfPath
        TIO.putStrLn $ "  " <> pfMsg
        pure (renderFailureBlock cfgSourceRoots src pfMsg)
      Right structural -> do
        modifyIORef' successesRef (+ 1)
        let merged   = applyPasses cfgAnnotations cfgTestRoots testIdx structural
            rendered = renderChaseFile merged
            drift    = checkAnnotationDrift merged
        forM_ drift \w ->
          putStrLn $ "  drift: " <> T.unpack w
        pure (renderBundleBlock cfgSourceRoots src rendered)

  successes <- readIORef successesRef
  failures  <- readIORef failuresRef
  preamble  <- renderPreamble cfgSourceRoots successes failures
  let bundle = preamble <> T.concat blocks
  TIO.writeFile bundlePath bundle
  when cfgVerbose $ do
    putStrLn $ "bundled " <> show successes
            <> " files (" <> show failures <> " parse failures)"
    putStrLn $ "write  " <> bundlePath

applyPasses
  :: Map Text ModuleAnnotations
  -> [FilePath]
  -> Coverage.TestRefIndex
  -> ChaseFile
  -> ChaseFile
applyPasses anns testRoots testIdx cf =
  let withAnn = attachAnnotations anns cf
  in if null testRoots
       then withAnn
       else Coverage.attachCoverage testIdx withAnn

renderPreamble :: [FilePath] -> Int -> Int -> IO Text
renderPreamble roots ok failed = do
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  pure $ T.unlines
    [ "%bundle chase v1"
    , "%generated " <> T.pack stamp
    , "%roots " <> T.intercalate ", " (map T.pack roots)
    , "%files " <> T.pack (show ok) <> " ok, "
                <> T.pack (show failed) <> " failed"
    , ""
    , "# This file is a chase bundle: structural skeletons of multiple"
    , "# Haskell and PureScript source files concatenated into one document."
    , "# Each block begins with === BEGIN <path> === on its own line."
    , "# Blocks contain the rendered .chase format: %file, %mod, %lang,"
    , "# %ext (Haskell only), %fixity, %uses, %const, %topology, data decls,"
    , "# %foreign (PureScript only), %pattern, type signatures with"
    , "# attached invariants and consumers, ? tested-by lines (when test"
    , "# coverage analysis was enabled), %decision blocks, %open_issue"
    , "# blocks (known problems with blocking/affects targets), and parse"
    , "# errors. Function bodies are deliberately omitted; the lines"
    , "# beginning with ! after a signature are the behavioral facts you"
    , "# would otherwise have to infer from reading the source."
    , ""
    ]

renderBundleBlock :: [FilePath] -> FilePath -> Text -> Text
renderBundleBlock roots src rendered =
  let rel = relativeToRoots roots src
  in T.unlines
       [ ""
       , "=== BEGIN " <> T.pack rel <> " ==="
       , ""
       ] <> rendered

renderFailureBlock :: [FilePath] -> FilePath -> Text -> Text
renderFailureBlock roots src msg =
  let rel = relativeToRoots roots src
  in T.unlines
       [ ""
       , "=== BEGIN " <> T.pack rel <> " ==="
       , ""
       , "%parse_error " <> msg
       , ""
       ]

attachAnnotations
  :: Map Text ModuleAnnotations -> ChaseFile -> ChaseFile
attachAnnotations annMap cf =
  case Map.lookup (chaseModuleName cf) annMap of
    Nothing -> cf
    Just ModuleAnnotations{..} -> cf
      { chaseConstants  = annConstants
      , chaseInvariants = annInvariants
      , chaseDecisions  = annDecisions
      , chaseOpenIssues = annOpenIssues
      , chaseTopologies = annTopologies
      }

-- | Drift checks over a merged ChaseFile.
--
-- Three families. NAME checks find annotations pointing at functions the
-- file does not define. The EMPTINESS check finds an invariant that
-- exists but says nothing: its lines introduce no name the signature did
-- not already carry. The FABRICATION check finds an open issue that
-- asserts something is absent when the file defines it.
--
-- Both of the latter were added on 2026-09-26 against measured output
-- from a local 7B annotating Pelotero.DB.Pool. Its first attempt produced
-- ten invariants of which eight were the type restated in English
-- ("runTransaction is a function that runs the given transaction using
-- the given connection pool"), and every name check passed. Told that its
-- invariants restated the signature, its next attempt deleted the
-- invariants and filed eight open issues instead, claiming among other
-- things that defaultDBConfig "is not implemented" and that loadDBConfig
-- "should read database configuration from environment variables", for
-- functions whose bodies are in the file. Both failures are mechanical
-- and neither was visible to the checker as it stood.
checkAnnotationDrift :: ChaseFile -> [Text]
checkAnnotationDrift ChaseFile{..} =
  let sigsByName =
        Map.fromList
          (  [ (sigName s, sigVerbatim s) | s <- chaseSignatures ]
          <> [ (sigName s, sigVerbatim s) | s <- chaseForeignImports ]
          <> [ (patName p, patVerbatim p) | p <- chasePatterns ]
          )
      validNames  = Map.keys sigsByName
      invMissing  =
        [ "invariant references unknown function: " <> invFunction i
        | i <- chaseInvariants
        , invFunction i `notElem` validNames
        ]
      decMissing  =
        [ "decision " <> decName d
            <> " references unknown function: " <> n
        | d <- chaseDecisions
        , n <- decAffects d
        , n `notElem` validNames
        ]
      issueMissing =
        [ "open_issue " <> oiName i
            <> " references unknown function: " <> n
        | i <- chaseOpenIssues
        , n <- oiAffects i
        , n `notElem` validNames
        ]
      invEmpty =
        [ "invariant for " <> fn
            <> " repeats the type. Replace it with a fact the type cannot carry:"
            <> " name another function it calls, a constant or setting it uses,"
            <> " an ordering that matters, or what happens in the failing case."
            <> " Keep the invariant; make it specific."
        | i <- chaseInvariants
        , let fn = invFunction i
        , Just sig <- [Map.lookup fn sigsByName]
        , not (null (invLines i))
        , not (saysSomethingNew fn sig (invLines i))
        ]
      issueFabricated =
        [ "open_issue " <> oiName i
            <> " says something is absent that this file defines ("
            <> T.intercalate ", " present
            <> "). An open issue records a hazard in code that EXISTS."
            <> " If the code is there, either describe the real hazard or"
            <> " return no open issue for it."
        | i <- chaseOpenIssues
        , claimsAbsence (oiWhat i) || claimsAbsence (oiWhy i)
        , let present = [ n | n <- oiAffects i, n `elem` validNames ]
        , not (null present)
        ]
  in invMissing <> decMissing <> issueMissing <> invEmpty <> issueFabricated

-- | How good a candidate annotation set is, for choosing between
-- attempts: the number of invariants that pass every check, and the
-- number of warnings. More good invariants wins; on a tie, fewer
-- warnings wins.
--
-- This exists because the retry loop used to keep the LAST attempt
-- unconditionally, and a 7B given a criticism of its invariants
-- responded by deleting them. An attempt that produces less than the one
-- before it is not an improvement, and the loop has to be able to say so.
annotationScore :: ChaseFile -> (Int, Int)
annotationScore cf@ChaseFile{..} =
  let sigsByName =
        Map.fromList
          (  [ (sigName s, sigVerbatim s) | s <- chaseSignatures ]
          <> [ (sigName s, sigVerbatim s) | s <- chaseForeignImports ]
          <> [ (patName p, patVerbatim p) | p <- chasePatterns ]
          )
      good =
        length
          [ ()
          | i <- chaseInvariants
          , Just sig <- [Map.lookup (invFunction i) sigsByName]
          , not (null (invLines i))
          , saysSomethingNew (invFunction i) sig (invLines i)
          ]
  in (good, length (checkAnnotationDrift cf))

-- | Whether an invariant's lines introduce at least one identifier that
-- the signature does not already contain.
--
-- The test is about NAMES rather than prose, because names are what a
-- reader cannot recover from the type: the other function called, the
-- constant used, the isolation level, the mode. "wraps runSession with
-- TxS.transaction ReadCommitted Write" names three; "runTransaction is a
-- function that runs the given transaction using the given connection
-- pool" names none.
--
-- KNOWN FALSE POSITIVE. An invariant whose new information is carried
-- entirely in English is flagged even though it is informative: "loads
-- the database configuration from environment variables" for
-- @loadDBConfig :: IO DBConfig@ adds a real fact this check cannot see.
-- That is why it produces a warning rather than a rejection, and why the
-- warning text asks for a specific fact rather than announcing a
-- failure.
saysSomethingNew :: Text -> Text -> [Text] -> Bool
saysSomethingNew fnName sig lns =
  let known = Set.insert (T.toLower fnName)
                (Set.map T.toLower (identifiersIn sig))
      said  = Set.map T.toLower (Set.unions (map identifiersIn lns))
  in not (Set.null (Set.difference said known))

-- | Whether a sentence asserts that something is missing or ought to
-- exist. Matched against real open issues from chase's own bundle (which
-- describe hazards in code that exists, and do not trip this) and
-- against the fabricated ones a 7B produced (which all do).
claimsAbsence :: Text -> Bool
claimsAbsence t =
  let l = T.toLower t
  in any (`T.isInfixOf` l)
       [ "not implemented", "is not provided", "are not provided"
       , "is missing", "are missing", "does not exist", "do not exist"
       , "needs to be implemented", "should be implemented"
       , "is not defined", "are not defined", "no implementation"
       , "should read", "should correctly", "should properly"
       , "should execute", "should convert", "should set up"
       , "should acquire", "should release"
       ]

-- | Identifier-shaped tokens: anything with a dot, an inner capital, a
-- leading capital, a digit or an underscore. Ordinary lower-case English
-- words are excluded, which is the point: they are what a restatement is
-- made of. Two characters is enough only when a digit and a letter are
-- both present, so "v2" counts and "is" does not.
identifiersIn :: Text -> Set Text
identifiersIn t =
  Set.fromList
    [ w
    | raw <- T.split (\c -> not (isAlphaNum c || c `elem` ("._'" :: String))) t
    , let w = T.dropAround (`elem` (".,;:!?'" :: String)) raw
    , T.length w >= 2
    , codeLike w
    ]
  where
    codeLike w
      | T.length w == 2 = T.any isDigit w && T.any isAlpha w
    codeLike w =
      T.any (== '.') (T.drop 1 (T.init w))
        || T.any isUpper (T.drop 1 w)
        || (T.any isUpper (T.take 1 w) && T.any isAlpha w)
        || T.any isDigit w
        || T.any (== '_') w

mkOutputPath :: FilePath -> [FilePath] -> FilePath -> FilePath
mkOutputPath outDir roots src =
  outDir </> dropExtension (relativeToRoots roots src) <.> "chase"

relativeToRoots :: [FilePath] -> FilePath -> FilePath
relativeToRoots roots src =
  let candidates =
        [ rel
        | root <- roots
        , let rel = makeRelative root src
        , not (equalFilePath rel src)
        , rel /= "."
        ]
  in case candidates of
       (r:_) -> r
       []    -> takeFileName src

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles root = do
  isFile <- doesFileExist root
  if isFile
    then pure [ root | takeExtension root `elem` [".hs", ".purs"] ]
    else do
      exists <- doesDirectoryExist root
      if not exists
        then pure []
        else do
          entries <- Data.List.sort <$> listDirectory root
          let fullPaths = map (root </>) entries
          results <- forM fullPaths \p -> do
            isDir <- doesDirectoryExist p
            if isDir
              then findSourceFiles p
              else pure [ p | takeExtension p `elem` [".hs", ".purs"] ]
          pure (concat results)