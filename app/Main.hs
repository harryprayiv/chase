{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (isSuffixOf)
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Chase
import Chase.Annotations.Json (loadAnnotations)

defaultAnnotationsPath :: FilePath
defaultAnnotationsPath = "chase-annotations.json"

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [src, out]               -> runWith src out Nothing
    [src, out, annPath]      -> runWith src out (Just annPath)
    _                         -> usage prog

runWith :: FilePath -> FilePath -> Maybe FilePath -> IO ()
runWith src out mExplicit = do
  annPath <- resolveAnnotationsPath mExplicit
  anns    <- loadAnnotationsOrDie annPath
  let bundleMode = ".chase" `isSuffixOf` out
      cfg | bundleMode = defaultConfig
              { cfgSourceRoots = [src]
              , cfgBundleFile  = Just out
              , cfgAnnotations = anns
              , cfgVerbose     = True
              }
          | otherwise = defaultConfig
              { cfgSourceRoots = [src]
              , cfgOutputDir   = out
              , cfgAnnotations = anns
              , cfgVerbose     = True
              }
  runChase cfg

-- | If the user gave an explicit path, use it (must exist).
-- Otherwise look for ./chase-annotations.json. Missing default is fine
-- (silent empty annotations); missing explicit is a user error.
resolveAnnotationsPath :: Maybe FilePath -> IO (Maybe FilePath)
resolveAnnotationsPath = \case
  Just p -> do
    exists <- doesFileExist p
    if exists
      then pure (Just p)
      else do
        hPutStrLn stderr $ "error: annotations file not found: " <> p
        exitFailure
  Nothing -> do
    exists <- doesFileExist defaultAnnotationsPath
    pure $ if exists then Just defaultAnnotationsPath else Nothing

loadAnnotationsOrDie
  :: Maybe FilePath -> IO (Map Text ModuleAnnotations)
loadAnnotationsOrDie Nothing = pure Map.empty
loadAnnotationsOrDie (Just p) = do
  result <- loadAnnotations p
  case result of
    Right m -> do
      putStrLn $ "load   " <> p <> " (" <> show (Map.size m) <> " modules)"
      pure m
    Left e -> do
      hPutStrLn stderr $ "error: " <> e
      exitFailure

usage :: String -> IO ()
usage prog = do
  hPutStrLn stderr $ "error: expected 2 or 3 arguments"
  hPutStrLn stderr ""
  mapM_ (hPutStrLn stderr)
    [ "usage: " <> prog <> " <source-root> <output> [<annotations.json>]"
    , ""
    , "  source-root        directory to walk for .hs files"
    , "  output             if it ends in .chase, written as a single"
    , "                     concatenated bundle file; otherwise treated"
    , "                     as a directory for per-file .chase output"
    , "  annotations.json   optional path to annotations JSON file."
    , "                     If omitted, ./chase-annotations.json is used"
    , "                     when present. If the explicit path is given"
    , "                     and missing, this is a hard error."
    , ""
    , "examples:"
    , "  " <> prog <> " backend/src cheeblr.chase"
    , "  " <> prog <> " backend/src cheeblr.chase ./chase-annotations.json"
    ]
  exitFailure