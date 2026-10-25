{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Options.Applicative
import           Control.Monad           (unless, filterM)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes)
import           Data.Text               (Text)
import           Data.List               (intercalate, isSuffixOf)
import           System.Directory        ( doesFileExist
                                         , doesDirectoryExist
                                         )
import           System.FilePath         ( takeDirectory
                                         , takeBaseName
                                         , (</>)
                                         )
import           GHC.IO.Encoding         (setLocaleEncoding, utf8)
import           System.IO               (hPutStrLn, hSetEncoding, stderr, stdin, stdout)
import           System.Exit             (exitFailure)

import qualified Chase.Pipeline          as Pipeline
import qualified Chase.Annotations.Json  as Anno
import           Chase.Inference         (findProjectRoot, inferenceDirName, locateAnnotations)
import           Chase.Types             (ModuleAnnotations)


data Options = Options
  { optSource      :: FilePath
  , optOutput      :: Maybe FilePath
  , optAnnotations :: AnnotationsMode
  , optTests       :: TestsMode
  , optHie         :: PathMode
  , optGhcLog      :: PathMode
  , optSkeleton    :: Maybe FilePath
  , optDerive      :: Bool
  , optQuiet       :: Bool
  }

data AnnotationsMode = AnnAuto | AnnFile FilePath | AnnOff
data TestsMode       = TestsAuto | TestsExplicit [FilePath] | TestsOff

-- | A path that is given, auto-detected under the project's .inference
-- directory, or off.
data PathMode = PathAuto | PathGiven FilePath | PathOff


main :: IO ()
main = utf8Everywhere >> execParser parserInfo >>= run

-- | Every file chase reads or writes is UTF-8, whatever the locale. GHC
-- opens files and the standard handles in the locale's encoding, and under
-- the POSIX locale that is ASCII: on 2026-10-24 ten of cheeblr's frontend
-- modules failed with "cannot decode byte sequence" because they hold a
-- character outside ASCII, and their annotations were reported as matching
-- no source file. A remote command run over ssh can get that locale.
utf8Everywhere :: IO ()
utf8Everywhere = do
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> options)
  ( fullDesc
 <> progDesc "Compress Haskell and PureScript source into LLM context skeletons."
 <> header "chase - structural source compressor"
  )

options :: Parser Options
options = Options
  <$> argument str
        ( metavar "SOURCE"
       <> help "File or directory to scan for .hs and .purs sources"
        )
  <*> optional
        ( strOption
            ( long "output"
           <> short 'o'
           <> metavar "PATH"
           <> help "Output path. Ending in .chase = bundle mode. \
                   \Anything else = per-file output directory. \
                   \Default: <source-basename>.chase in CWD."
            )
        )
  <*> annotationsOpt
  <*> testsOpt
  <*> pathOpt "hie" "DIR"
        "Directory holding the project's .hie files, written by its own \
        \build with -fwrite-ide-info. Callers are then resolved by the \
        \compiler, and the uses, unreferenced and constructor-usage facts \
        \are added. Default: .inference/hie in the project SOURCE is in, \
        \when it exists."
        "Do not read .hie files even if .inference/hie exists."
  <*> pathOpt "ghc-log" "FILE"
        "A GHC build log. Its warnings (unhandled constructors, unreachable \
        \alternatives, partial fields) are attached to the signatures they \
        \point at. Default: .inference/build.log in the project SOURCE is \
        \in, when it exists."
        "Do not read a build log even if .inference/build.log exists."
  <*> optional
        ( strOption
            ( long "skeleton"
           <> metavar "FILE"
           <> help "Add a skeleton annotations entry to FILE for every \
                   \parsed module FILE does not have yet: invariants with \
                   \requires, fails, effects and consumes filled from the \
                   \derived facts and an empty intent, constants from the \
                   \candidates, topologies from the derived edges with an \
                   \empty terminal list. Modules already in FILE are kept \
                   \as they are."
            )
        )
  <*> fmap not
        ( switch
            ( long "no-derive"
           <> help "Skip the derive pass: no ~ lines, no callers, no \
                   \derived transitions, no topology or consumes checks."
            )
        )
  <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Suppress progress output."
        )
  where
    annotationsOpt =
          ( AnnFile <$> strOption
              ( long "annotations"
             <> short 'a'
             <> metavar "FILE"
             <> help "Annotations JSON path. \
                     \Default: chase-annotation.json or \
                     \chase-annotations.json in the .inference directory \
                     \of the project SOURCE is in (the nearest directory \
                     \above SOURCE that has one, within its repository)."
              )
          )
      <|> flag' AnnOff
              ( long "no-annotations"
             <> help "Disable annotation enrichment."
              )
      <|> pure AnnAuto

    testsOpt =
          ( TestsExplicit . splitCommas <$> strOption
              ( long "tests"
             <> short 't'
             <> metavar "DIR[,DIR..]"
             <> help "Test root(s) to scan for references. \
                     \Default: auto-detect test/, tests/, spec/ \
                     \adjacent to source."
              )
          )
      <|> flag' TestsOff
              ( long "no-tests"
             <> help "Disable test reference scanning."
              )
      <|> pure TestsAuto

    pathOpt name meta onHelp offHelp =
          ( PathGiven <$> strOption (long name <> metavar meta <> help onHelp) )
      <|> flag' PathOff (long ("no-" <> name) <> help offHelp)
      <|> pure PathAuto


run :: Options -> IO ()
run opts = do
  let source = optSource opts
  srcIsFile <- doesFileExist source
  srcIsDir  <- doesDirectoryExist source
  unless (srcIsFile || srcIsDir) $ do
    hPutStrLn stderr $ "chase: source not found: " <> source
    exitFailure

  let output = case optOutput opts of
        Just o  -> o
        Nothing -> takeBaseName source <> ".chase"

  annPath  <- resolveAnnotations source srcIsDir (optAnnotations opts)
  annMap   <- loadAnnotationsOrDie (optQuiet opts) annPath
  testDirs <- resolveTests source srcIsDir (optTests opts)
  hieDir   <- resolvePath source doesDirectoryExist "hie directory" "hie" (optHie opts)
  ghcLog   <- resolvePath source doesFileExist "build log" "build.log" (optGhcLog opts)

  let isBundle = ".chase" `isSuffixOf` output
      cfg = Pipeline.defaultConfig
              { Pipeline.cfgSourceRoots = [source]
              , Pipeline.cfgOutputDir   = if isBundle then "chase" else output
              , Pipeline.cfgBundleFile  = if isBundle then Just output else Nothing
              , Pipeline.cfgAnnotations = annMap
              , Pipeline.cfgTestRoots   = testDirs
              , Pipeline.cfgVerbose     = not (optQuiet opts)
              , Pipeline.cfgDerive      = optDerive opts
              , Pipeline.cfgHieDir      = hieDir
              , Pipeline.cfgGhcLog      = ghcLog
              , Pipeline.cfgSkeleton    = optSkeleton opts
              }
  Pipeline.runChase cfg


resolveAnnotations
  :: FilePath -> Bool -> AnnotationsMode -> IO (Maybe FilePath)
resolveAnnotations _ _ AnnOff      = pure Nothing
resolveAnnotations _ _ (AnnFile f) = do
  ok <- doesFileExist f
  if ok then pure (Just f) else do
    hPutStrLn stderr $ "chase: annotations file not found: " <> f
    exitFailure
resolveAnnotations source _ AnnAuto = locateAnnotations source


resolveTests :: FilePath -> Bool -> TestsMode -> IO [FilePath]
resolveTests _ _ TestsOff           = pure []
resolveTests _ _ (TestsExplicit ds) = catMaybes <$> mapM checkDir ds
  where
    checkDir d = do
      ok <- doesDirectoryExist d
      if ok then pure (Just d) else do
        hPutStrLn stderr $ "chase: warning: test dir not found: " <> d
        pure Nothing
resolveTests source isDir TestsAuto = do
  let sourceDir
        | isDir     = source
        | otherwise = takeDirectory source
      parent     = takeDirectory sourceDir
      candidates = [parent </> "test", parent </> "tests", parent </> "spec"]
  found <- filterM doesDirectoryExist candidates
  case found of
    [] -> pure []
    ds -> do
      hPutStrLn stderr $ "chase: test roots " <> intercalate ", " ds
      pure ds


-- | A path under .inference: the one given, which must exist, or the
-- named entry in the project's .inference directory when that exists,
-- or none.
resolvePath :: FilePath -> (FilePath -> IO Bool) -> String -> FilePath -> PathMode -> IO (Maybe FilePath)
resolvePath _ _ _ _ PathOff = pure Nothing
resolvePath _ exists what _ (PathGiven p) = do
  ok <- exists p
  if ok then pure (Just p) else do
    hPutStrLn stderr $ "chase: " <> what <> " not found: " <> p
    exitFailure
resolvePath source exists _ name PathAuto =
  findProjectRoot source >>= \case
    Nothing   -> pure Nothing
    Just root -> do
      let p = root </> inferenceDirName </> name
      ok <- exists p
      pure (if ok then Just p else Nothing)


loadAnnotationsOrDie
  :: Bool -> Maybe FilePath -> IO (Map Text ModuleAnnotations)
loadAnnotationsOrDie _     Nothing  = pure Map.empty
loadAnnotationsOrDie quiet (Just p) = do
  res <- Anno.loadAnnotations p
  case res of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right m -> do
      unless quiet $
        putStrLn $ "load   " <> p
                <> " (" <> show (Map.size m) <> " modules)"
      pure m


splitCommas :: String -> [String]
splitCommas s = case break (== ',') s of
  (before, [])      -> [before]
  (before, _:after) -> before : splitCommas after