{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE MultiWayIf        #-}

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
import System.Directory
  ( createDirectoryIfMissing )
import System.FilePath
  ( (</>), (<.>), takeDirectory, takeExtension, takeFileName
  , dropExtension, makeRelative, equalFilePath
  )
import qualified Data.List

import           Chase.Types
import           Chase.Parse
import           Chase.Render
import qualified Chase.Callers       as Callers
import qualified Chase.Coverage      as Coverage
import           Chase.Derive
import           Chase.Derive.GhcLog (attachGhcWarnings, parseGhcLog, unattached)
import           Chase.Derive.Hie    (constructorUsage, hieCallerIndex, hieFactsFor)
import           Chase.Hie           (HieModule, readHieDir)
import           Chase.Sources       (findSourceFiles)

data ChaseConfig = ChaseConfig
  { cfgSourceRoots :: [FilePath]
  , cfgOutputDir   :: FilePath
  , cfgBundleFile  :: Maybe FilePath
  , cfgAnnotations :: Map Text ModuleAnnotations
  , cfgTestRoots   :: [FilePath]
  , cfgVerbose     :: Bool
  , cfgDerive      :: Bool
  -- ^ run the derive pass: clause, fails and literal lines, constant
  -- candidates, abstract types, callers, derived transitions and the
  -- checks of %topology and consumes against them
  , cfgHieDir      :: Maybe FilePath
  -- ^ a directory holding the project's .hie files; when given, callers
  -- are resolved from them and the HIE facts are attached
  , cfgGhcLog      :: Maybe FilePath
  -- ^ a GHC build log; its warnings are attached to the signatures they
  -- point at
  }

defaultConfig :: ChaseConfig
defaultConfig = ChaseConfig
  { cfgSourceRoots = []
  , cfgOutputDir   = "chase"
  , cfgBundleFile  = Nothing
  , cfgAnnotations = Map.empty
  , cfgTestRoots   = []
  , cfgVerbose     = False
  , cfgDerive      = True
  , cfgHieDir      = Nothing
  , cfgGhcLog      = Nothing
  }

-- | One source file after parsing.
data Item
  = ItemFailed FilePath Text
  | ItemOk FilePath Parsed

-- | Everything the per-file passes need that is known only once every
-- file has been parsed.
data RunContext = RunContext
  { rcTestIndex  :: Coverage.TestRefIndex
  , rcCallers    :: Maybe RefIndex
  , rcTopologies :: [DerivedTopology]
  , rcConMap     :: ConMap
  , rcHie        :: [HieModule]
  , rcUsage      :: [ConstructorUsage]
  , rcWarnings   :: [GhcWarning]
  }

runChase :: ChaseConfig -> IO ()
runChase cfg = do
  testIdx  <- buildTestIndexOrEmpty cfg
  items    <- parseAll cfg
  hie      <- readHie cfg
  warnings <- readGhcLog cfg
  let oks     = [ p | ItemOk _ p <- items ]
      conMap  = buildConMap oks
      derived = [ parsedDerive p conMap | p <- oks ]
      tops    = if cfgDerive cfg then mergeTopologies oks derived else []
  callers <-
    if | not (cfgDerive cfg) -> pure Nothing
       | not (null hie) -> pure (Just (hieCallerIndex hie))
       | otherwise ->
           Just <$> Callers.buildCallerIndex
             (cfgVerbose cfg)
             Callers.ScanPlan
               { Callers.spRoots  = cfgSourceRoots cfg
               , Callers.spSource = "the run's source roots"
               }
  let ctx = RunContext
        { rcTestIndex  = testIdx
        , rcCallers    = callers
        , rcTopologies = tops
        , rcConMap     = conMap
        , rcHie        = hie
        , rcUsage      = constructorUsage hie
        , rcWarnings   = warnings
        }
      stray = unattached warnings (map parsedFile oks)
  when (not (null stray)) $
    putStrLn $ "ghc    " <> show (length stray) <> " warnings point at files outside this run"
  parsed <- case cfgBundleFile cfg of
    Nothing     -> runPerFile  cfg ctx items
    Just bundle -> runBundled  cfg ctx items bundle
  forM_ (unmatchedAnnotations (cfgAnnotations cfg) parsed) \w ->
    putStrLn $ "  drift: " <> T.unpack w

-- | The HIE files, when a directory was given. Files that cannot be read
-- are reported one per line; a GHC version other than chase's own is the
-- usual cause.
readHie :: ChaseConfig -> IO [HieModule]
readHie ChaseConfig{..} = case cfgHieDir of
  Nothing  -> pure []
  Just dir -> do
    (mods, warnings) <- readHieDir dir
    forM_ warnings \w -> TIO.putStrLn ("WARN: hie: " <> w)
    putStrLn $ "hie    " <> show (length mods) <> " modules from " <> dir
    pure mods

-- | The compiler's warnings, when a build log was given.
readGhcLog :: ChaseConfig -> IO [GhcWarning]
readGhcLog ChaseConfig{..} = case cfgGhcLog of
  Nothing   -> pure []
  Just path -> do
    ws <- parseGhcLog <$> TIO.readFile path
    putStrLn $ "ghc    " <> show (length ws) <> " warnings from " <> path
    pure ws

-- | Parse every file under the source roots once. Failures are reported
-- here and carried as items so the bundle can hold a block for them.
parseAll :: ChaseConfig -> IO [Item]
parseAll ChaseConfig{..} = do
  files <- concat <$> mapM findSourceFiles cfgSourceRoots
  forM files \src -> do
    when cfgVerbose $ putStrLn $ "parse  " <> src
    result <- parseSourceFileFull src
    case result of
      Left ParseFailure{..} -> do
        putStrLn $ "WARN: parse failed for " <> pfPath
        TIO.putStrLn $ "  " <> pfMsg
        pure (ItemFailed src pfMsg)
      Right p -> pure (ItemOk src p)

-- | Annotated modules that no parsed file defines: renamed or deleted
-- modules whose annotations would otherwise stay in the file and attach to
-- nothing, without a word. A module whose file failed to parse is not
-- listed here, since its parse failure is already reported.
unmatchedAnnotations :: Map Text ModuleAnnotations -> Set Text -> [Text]
unmatchedAnnotations anns parsed =
  [ "annotations for module " <> m <> " match no source file"
  | m <- Map.keys anns
  , not (Set.member m parsed)
  ]

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

-- | The merged file and its drift warnings, for one parsed item.
finishFile :: ChaseConfig -> RunContext -> Parsed -> (ChaseFile, [Text])
finishFile ChaseConfig{..} RunContext{..} p =
  let structural = parsedFile p
      withDerived =
        if cfgDerive
          then structural { chaseDerived = Just (parsedDerive p rcConMap) }
          else structural
      withHie  = case rcCallers of
        Just idx | not (null rcHie) ->
          withDerived { chaseHie = hieFactsFor rcHie idx (chaseModuleName structural) }
        _ -> withDerived
      withGhc  = attachGhcWarnings rcWarnings withHie
      withAnn  = attachAnnotations cfgAnnotations withGhc
      withCov  = if null cfgTestRoots then withAnn else Coverage.attachCoverage rcTestIndex withAnn
      merged   = case rcCallers of
        Just idx -> attachCallers idx withCov
        Nothing  -> withCov
      drift    =
           checkAnnotationDrift merged
        <> (if cfgDerive then checkTopologyDrift rcTopologies merged else [])
        <> maybe [] (`checkConsumesDrift` merged) rcCallers
  in (merged, drift)

-- | Writes one .chase per source file, plus DERIVED.chase for the
-- run-wide transitions and constructor usage when there are any. Returns
-- the modules parsed, and likewise for 'runBundled'.
runPerFile :: ChaseConfig -> RunContext -> [Item] -> IO (Set Text)
runPerFile cfg@ChaseConfig{..} ctx items = do
  createDirectoryIfMissing True cfgOutputDir
  parsed <- fmap (Set.fromList . concat) . forM items $ \case
    ItemFailed _ _ -> pure []
    ItemOk src p -> do
      let (merged, drift) = finishFile cfg ctx p
          outPath  = mkOutputPath cfgOutputDir cfgSourceRoots src
          rendered = renderChaseFile merged
      forM_ drift \w ->
        putStrLn $ "  drift: " <> T.unpack w
      createDirectoryIfMissing True (takeDirectory outPath)
      TIO.writeFile outPath rendered
      when cfgVerbose $ putStrLn $ "write  " <> outPath
      pure [chaseModuleName merged]
  let tail' = runTail ctx
  when (not (T.null tail')) $ do
    let derivedPath = cfgOutputDir </> "DERIVED.chase"
    TIO.writeFile derivedPath tail'
    when cfgVerbose $ putStrLn $ "write  " <> derivedPath
  pure parsed

runBundled :: ChaseConfig -> RunContext -> [Item] -> FilePath -> IO (Set Text)
runBundled cfg@ChaseConfig{..} ctx items bundlePath = do
  createDirectoryIfMissing True (takeDirectory bundlePath)
  blocks <- forM items \case
    ItemFailed src msg ->
      pure (Nothing, renderFailureBlock cfgSourceRoots src msg)
    ItemOk src p -> do
      let (merged, drift) = finishFile cfg ctx p
          rendered = renderChaseFile merged
      forM_ drift \w ->
        putStrLn $ "  drift: " <> T.unpack w
      pure (Just (chaseModuleName merged), renderBundleBlock cfgSourceRoots src rendered)
  let successes = length [ () | (Just _, _) <- blocks ]
      failures  = length [ () | (Nothing, _) <- blocks ]
      bundle = renderPreamble cfgSourceRoots successes failures
            <> T.concat (map snd blocks)
            <> runTail ctx
  TIO.writeFile bundlePath bundle
  when cfgVerbose $ do
    putStrLn $ "bundled " <> show successes
            <> " files (" <> show failures <> " parse failures)"
    putStrLn $ "write  " <> bundlePath
  pure (Set.fromList [ m | (Just m, _) <- blocks ])

-- | The run-wide sections that follow the per-file blocks.
runTail :: RunContext -> Text
runTail RunContext{..} =
  renderDerivedTopologies rcTopologies <> renderConstructorUsage rcUsage

-- | The bundle's header. Nothing in it depends on when chase ran, so the
-- same sources, annotations and test roots give the same bytes. A
-- %generated timestamp stood second until 2026-10-24; it made every
-- bundle new to open-slop's ingest, which skips a file whose content hash
-- it has already stored, so every bundle was re-embedded on every ingest.
renderPreamble :: [FilePath] -> Int -> Int -> Text
renderPreamble roots ok failed =
  T.unlines
    [ "%bundle chase v1"
    , "%roots " <> T.intercalate ", " (map T.pack roots)
    , "%files " <> T.pack (show ok) <> " ok, "
                <> T.pack (show failed) <> " failed"
    , ""
    , "# This file is a chase bundle: structural skeletons of multiple"
    , "# Haskell and PureScript source files concatenated into one document."
    , "# Each block begins with === BEGIN <path> === on its own line."
    , "# Blocks contain the rendered .chase format: %file, %mod, %lang,"
    , "# %ext (Haskell only), %ghc, %fixity, %uses, %const, %const_candidate,"
    , "# %topology, %abstract, %untested, data decls, %foreign (PureScript"
    , "# only), %pattern, type signatures with attached lines, %decision"
    , "# blocks, %open_issue blocks (known problems with blocking/affects"
    , "# targets), and parse errors. Function bodies are deliberately omitted."
    , "#"
    , "# Lines under a signature, by sigil:"
    , "#   !  a hand-written invariant: a behavioral fact the type does not carry"
    , "#   >  a hand-written consumer of the function or its side effects"
    , "#   ~  a fact chase read from the code itself. From the parse these are"
    , "#      may-facts: a clause may be unreachable, a failure may be on a"
    , "#      dead path, a literal may be harmless."
    , "#      clause: the constructor or literal patterns and guards of one clause"
    , "#      fails: error constructors or strings raised under throwError,"
    , "#        throwIO, throwM, throw, throwE, Left, fail, error, exitWith"
    , "#      literals: literals in the body other than 0, 1 and \"\""
    , "#      uses: functions from outside the project that the body references,"
    , "#        resolved by the compiler (HIE); the side effects the type may hide"
    , "#      unreferenced: not exported and nothing in the project references it (HIE)"
    , "#      ghc: the compiler's own warning at this function, from the build log:"
    , "#        unhandled constructors, unreachable alternatives, partial fields"
    , "#      called by: functions that reference this one; resolved by the"
    , "#        compiler when HIE files were read, by import rules otherwise"
    , "#   ?  test functions that reference the name (when test roots were scanned)"
    , "#"
    , "# %untested lists the names no test references. %ghc at the top of a"
    , "# block is a compiler warning above every signature of the file."
    , "# %abstract names a data type exported without all its constructors and"
    , "# the exported functions that return it: build values of it through"
    , "# those functions, since the constructors are not reachable from outside."
    , "# %const_candidate is a zero-argument literal binding with no %const note."
    , "# A === DERIVED TRANSITIONS === block at the end lists, per sum type,"
    , "# the constructor-to-constructor edges found in clauses and case"
    , "# alternatives, each with the function that witnesses it. A"
    , "# === CONSTRUCTOR USAGE === block lists constructors nothing builds and"
    , "# constructors of sum types nothing matches, from the HIE files."
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
--
-- The derive pass adds its own checks in Chase.Derive (topology edges
-- against derived edges, consumes against callers); the pipeline runs
-- them next to these.
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