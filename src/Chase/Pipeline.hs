{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BlockArguments    #-}

module Chase.Pipeline
  ( ChaseConfig (..)
  , defaultConfig
  , runChase
  , attachAnnotations
  , checkAnnotationDrift
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM, forM_, when)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, listDirectory )
import System.FilePath
  ( (</>), (<.>), takeDirectory, takeExtension
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

-- | Parse every .hs file under cfgTestRoots and build the global
-- reference index. If cfgTestRoots is empty, returns an empty map and
-- the rest of the pipeline emits no ? lines.
buildTestIndexOrEmpty :: ChaseConfig -> IO Coverage.TestRefIndex
buildTestIndexOrEmpty ChaseConfig{..}
  | null cfgTestRoots = pure Map.empty
  | otherwise = do
      when cfgVerbose $ do
        putStrLn ""
        putStrLn $ "test roots: "
                <> Data.List.intercalate ", " cfgTestRoots
      files <- concat <$> mapM findSourceFiles cfgTestRoots
      let hsFiles = filter (\p -> takeExtension p == ".hs") files
      bindings <- forM hsFiles \p -> do
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

-- | Compose attachAnnotations and attachCoverage; the latter only runs
-- if cfgTestRoots is non-empty (the testIdx is meaningless otherwise).
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

checkAnnotationDrift :: ChaseFile -> [Text]
checkAnnotationDrift ChaseFile{..} =
  let validNames  = map sigName chaseSignatures
                 <> map sigName chaseForeignImports
                 <> map patName chasePatterns
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
  in invMissing <> decMissing <> issueMissing

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
        ]
  in case candidates of
       (r:_) -> r
       []    -> src

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles root = do
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