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

import Chase.Types
import Chase.Parse
import Chase.Render

data ChaseConfig = ChaseConfig
  { cfgSourceRoots :: [FilePath]
    -- ^ source directories to walk, e.g. @["backend\/src"]@
  , cfgOutputDir   :: FilePath
    -- ^ where to write per-file .chase output (used only when
    --   cfgBundleFile is Nothing)
  , cfgBundleFile  :: Maybe FilePath
    -- ^ if Just, write a single concatenated bundle to this path
    --   instead of a per-file tree
  , cfgAnnotations :: Map Text ModuleAnnotations
    -- ^ keyed by module name, e.g. @"DB.Auth"@
  , cfgVerbose     :: Bool
  }

defaultConfig :: ChaseConfig
defaultConfig = ChaseConfig
  { cfgSourceRoots = []
  , cfgOutputDir   = "chase"
  , cfgBundleFile  = Nothing
  , cfgAnnotations = Map.empty
  , cfgVerbose     = False
  }

runChase :: ChaseConfig -> IO ()
runChase cfg@ChaseConfig{..} = case cfgBundleFile of
  Nothing      -> runPerFile cfg
  Just bundle  -> runBundled cfg bundle

-- per-file mode: original behavior, one .chase per source file -----------

runPerFile :: ChaseConfig -> IO ()
runPerFile ChaseConfig{..} = do
  createDirectoryIfMissing True cfgOutputDir
  files <- concat <$> mapM findHaskellFiles cfgSourceRoots
  forM_ files \src -> do
    when cfgVerbose $ putStrLn $ "parse  " <> src
    result <- parseSourceFile src
    case result of
      Left ParseFailure{..} -> do
        putStrLn $ "WARN: parse failed for " <> pfPath
        TIO.putStrLn $ "  " <> pfMsg
      Right structural -> do
        let merged   = attachAnnotations cfgAnnotations structural
            outPath  = mkOutputPath cfgOutputDir cfgSourceRoots src
            rendered = renderChaseFile merged
            drift    = checkAnnotationDrift merged
        forM_ drift \w ->
          putStrLn $ "  drift: " <> T.unpack w
        createDirectoryIfMissing True (takeDirectory outPath)
        TIO.writeFile outPath rendered
        when cfgVerbose $ putStrLn $ "write  " <> outPath

-- bundled mode: one concatenated file with separators -------------------

runBundled :: ChaseConfig -> FilePath -> IO ()
runBundled ChaseConfig{..} bundlePath = do
  createDirectoryIfMissing True (takeDirectory bundlePath)
  files <- concat <$> mapM findHaskellFiles cfgSourceRoots
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
        let merged   = attachAnnotations cfgAnnotations structural
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
    , "# Haskell source files concatenated into one document. Each block"
    , "# begins with === BEGIN <path> === on its own line. Blocks contain"
    , "# the rendered .chase format: %file, %mod, %uses, type signatures,"
    , "# data declarations, and any attached invariants/decisions/"
    , "# topologies. Function bodies are deliberately omitted; the lines"
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

-- shared helpers -------------------------------------------------------

attachAnnotations
  :: Map Text ModuleAnnotations -> ChaseFile -> ChaseFile
attachAnnotations annMap cf =
  case Map.lookup (chaseModuleName cf) annMap of
    Nothing -> cf
    Just ModuleAnnotations{..} -> cf
      { chaseConstants  = annConstants
      , chaseInvariants = annInvariants
      , chaseDecisions  = annDecisions
      , chaseTopologies = annTopologies
      }

-- | Find function names referenced by invariants or decisions
-- that don\'t actually exist in the parsed signatures. This is the
-- sidecar equivalent of compile-time function name checking: not
-- as strong, but caught early enough to fix.
checkAnnotationDrift :: ChaseFile -> [Text]
checkAnnotationDrift ChaseFile{..} =
  let sigNames    = map sigName chaseSignatures
      invMissing  =
        [ "invariant references unknown function: " <> invFunction i
        | i <- chaseInvariants
        , invFunction i `notElem` sigNames
        ]
      decMissing  =
        [ "decision " <> decName d
            <> " references unknown function: " <> n
        | d <- chaseDecisions
        , n <- decAffects d
        , n `notElem` sigNames
        ]
  in invMissing <> decMissing

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

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- Data.List.sort <$> listDirectory root
      let fullPaths = map (root </>) entries
      results <- forM fullPaths \p -> do
        isDir <- doesDirectoryExist p
        if isDir
          then findHaskellFiles p
          else pure [ p | takeExtension p == ".hs" ]
      pure (concat results)