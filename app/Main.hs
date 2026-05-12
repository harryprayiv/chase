{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.List (isSuffixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.IO (hPutStrLn, stderr)

import Chase.Pipeline
import Chase.Annotations.Json
import Chase.Types

defaultAnnotationsPath :: FilePath
defaultAnnotationsPath = "chase-annotations.json"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, out] ->
      runWith src out Nothing Nothing
    [src, out, ann] ->
      runWith src out (Just ann) Nothing
    [src, out, ann, test] ->
      runWith src out (Just ann) (Just test)
    _ -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr "usage: chase <source-root> <output> [<annotations.json>] [<test-root>]"
  hPutStrLn stderr ""
  hPutStrLn stderr "  source-root        directory to scan for .hs and .purs files"
  hPutStrLn stderr "  output             .chase file for bundle mode, or a directory for per-file mode"
  hPutStrLn stderr "  annotations.json   optional; auto-detected from ./chase-annotations.json"
  hPutStrLn stderr "  test-root          optional; if given, scan .hs files there for"
  hPutStrLn stderr "                     test references to source functions (Haskell only)"
  exitFailure

runWith :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO ()
runWith srcRoot output mAnn mTest = do
  resolvedAnn  <- resolveAnnotationsPath mAnn
  annMap       <- loadAnnotationsOrDie resolvedAnn
  resolvedTest <- resolveTestRoot mTest
  let cfg = defaultConfig
        { cfgSourceRoots = [srcRoot]
        , cfgOutputDir   = if ".chase" `isSuffixOf` output
                              then "chase"
                              else output
        , cfgBundleFile  = if ".chase" `isSuffixOf` output
                              then Just output
                              else Nothing
        , cfgAnnotations = annMap
        , cfgTestRoots   = maybe [] (:[]) resolvedTest
        , cfgVerbose     = True
        }
  runChase cfg

resolveAnnotationsPath :: Maybe FilePath -> IO (Maybe FilePath)
resolveAnnotationsPath = \case
  Just p -> do
    exists <- doesFileExist p
    if exists
      then pure (Just p)
      else do
        hPutStrLn stderr $ "annotations file does not exist: " <> p
        exitFailure
  Nothing -> do
    exists <- doesFileExist defaultAnnotationsPath
    pure $ if exists then Just defaultAnnotationsPath else Nothing

resolveTestRoot :: Maybe FilePath -> IO (Maybe FilePath)
resolveTestRoot = \case
  Just p -> do
    exists <- doesDirectoryExist p
    if exists
      then pure (Just p)
      else do
        hPutStrLn stderr $ "test root directory does not exist: " <> p
        exitFailure
  Nothing -> pure Nothing

loadAnnotationsOrDie :: Maybe FilePath -> IO (Map Text ModuleAnnotations)
loadAnnotationsOrDie = \case
  Nothing -> pure Map.empty
  Just p  -> do
    res <- loadAnnotations p
    case res of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right m -> do
        putStrLn $ "load   " <> p <> " (" <> show (Map.size m) <> " modules)"
        pure m