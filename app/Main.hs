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
                                         , getCurrentDirectory
                                         )
import           System.FilePath         ( takeDirectory
                                         , takeBaseName
                                         , (</>)
                                         )
import           System.IO               (hPutStrLn, stderr)
import           System.Exit             (exitFailure)

import qualified Chase.Pipeline          as Pipeline
import qualified Chase.Annotations.Json  as Anno
import           Chase.Types             (ModuleAnnotations)


data Options = Options
  { optSource      :: FilePath
  , optOutput      :: Maybe FilePath
  , optAnnotations :: AnnotationsMode
  , optTests       :: TestsMode
  , optQuiet       :: Bool
  }

data AnnotationsMode = AnnAuto | AnnFile FilePath | AnnOff
data TestsMode       = TestsAuto | TestsExplicit [FilePath] | TestsOff


main :: IO ()
main = execParser parserInfo >>= run

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
                     \Default: auto-detect chase-annotation.json \
                     \or chase-annotations.json in CWD, source dir, \
                     \or source parent."
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

  let isBundle = ".chase" `isSuffixOf` output
      cfg = Pipeline.defaultConfig
              { Pipeline.cfgSourceRoots = [source]
              , Pipeline.cfgOutputDir   = if isBundle then "chase" else output
              , Pipeline.cfgBundleFile  = if isBundle then Just output else Nothing
              , Pipeline.cfgAnnotations = annMap
              , Pipeline.cfgTestRoots   = testDirs
              , Pipeline.cfgVerbose     = not (optQuiet opts)
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
resolveAnnotations source isDir AnnAuto = do
  cwd <- getCurrentDirectory
  let sourceDir
        | isDir     = source
        | otherwise = takeDirectory source
      parent     = takeDirectory sourceDir
      dirs       = [cwd, sourceDir, parent]
      names      = ["chase-annotation.json", "chase-annotations.json"]
      candidates = [d </> n | d <- dirs, n <- names]
  findFirst doesFileExist candidates


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


findFirst :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findFirst _ []     = pure Nothing
findFirst p (x:xs) = do
  ok <- p x
  if ok then pure (Just x) else findFirst p xs

splitCommas :: String -> [String]
splitCommas s = case break (== ',') s of
  (before, [])      -> [before]
  (before, _:after) -> before : splitCommas after