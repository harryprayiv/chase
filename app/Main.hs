module Main (main) where

import qualified Data.Map.Strict as Map
import Data.List (isSuffixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Chase

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [src, out]
      | ".chase" `isSuffixOf` out ->
          runChase defaultConfig
            { cfgSourceRoots = [src]
            , cfgBundleFile  = Just out
            , cfgAnnotations = Map.empty
            , cfgVerbose     = True
            }
      | otherwise ->
          runChase defaultConfig
            { cfgSourceRoots = [src]
            , cfgOutputDir   = out
            , cfgAnnotations = Map.empty
            , cfgVerbose     = True
            }
    _ -> do
      hPutStrLn stderr $ "error: expected 2 arguments, got " <> show (length args)
      hPutStrLn stderr ""
      mapM_ (hPutStrLn stderr)
        [ "usage: " <> prog <> " <source-root> <output>"
        , ""
        , "  source-root   directory to walk for .hs files"
        , "  output        if it ends in .chase, written as a single"
        , "                concatenated bundle file; otherwise treated"
        , "                as a directory for per-file .chase output"
        , ""
        , "examples:"
        , "  " <> prog <> " backend/src chase-output"
        , "  " <> prog <> " backend/src cheeblr.chase"
        , ""
        , "This bare runner emits structure only. For annotated output,"
        , "write your own runner that imports chase as a library:"
        , ""
        , "  module Main where"
        , "  import qualified Data.Map.Strict as Map"
        , "  import Chase"
        , "  import qualified Annotations.DB.Auth as DBAuth"
        , ""
        , "  main = runChase defaultConfig"
        , "    { cfgSourceRoots = [\"backend/src\"]"
        , "    , cfgBundleFile  = Just \"cheeblr.chase\""
        , "    , cfgAnnotations = DBAuth.annotations"
        , "    , cfgVerbose     = True"
        , "    }"
        ]
      exitFailure