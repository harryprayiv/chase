{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chase.Annotations.Json (loadAnnotationsIfExists)
import Chase.Inference (findProjectRoot, inferenceDirName, unisonAnnotationFileName)
import Chase.Types (emptyAnnotations)
import Chase.Unison.Annotate
import Chase.Unison.Api
import Chase.Unison.Extract
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= \case
  ("list" : url : project : branch : rest) ->
    runList url project branch (optArg rest)
  ("extract" : url : project : branch : rest) ->
    let (mOut, mAnn, positional) = parseExtractFlags rest
    in runExtract url project branch (maybe "" id (optArg positional)) mOut mAnn
  _ -> usage

-- pull '-o OUT' and '-a ANN' out of the args; the rest stays positional
parseExtractFlags :: [String] -> (Maybe FilePath, Maybe FilePath, [String])
parseExtractFlags = go (Nothing, Nothing, [])
  where
    go (o, a, ps) []                = (o, a, reverse ps)
    go (_, a, ps) ("-o" : x : rest) = go (Just x, a, ps) rest
    go (o, _, ps) ("-a" : x : rest) = go (o, Just x, ps) rest
    go (o, a, ps) (x : rest)        = go (o, a, x : ps) rest

optArg :: [String] -> Maybe T.Text
optArg (x : _) = Just (T.pack x)
optArg []      = Nothing

-- | The annotations file when -a is not given: .inference/chase-unison-annotations.json
-- in the project chase-unison is run from. A codebase is not a directory, so
-- the working directory is the only place a project can be found from;
-- run chase-unison from the project's checkout. Outside any project with a
-- .inference directory there is no default, and the bundle has no
-- annotations.
defaultAnnPath :: IO (Maybe FilePath)
defaultAnnPath = fmap (\root -> root </> inferenceDirName </> unisonAnnotationFileName) <$> findProjectRoot "."

usage :: IO ()
usage = do
  putStrLn "usage:"
  putStrLn "  chase-unison list    <baseUrl> <project> <branch> [namespace]"
  putStrLn "  chase-unison extract <baseUrl> <project> <branch> [root] [-o OUT.chase] [-a ANN.json]"
  putStrLn ""
  putStrLn "  baseUrl is scheme://host:port/token, e.g. http://127.0.0.1:5858/codebase"
  putStrLn "  -o writes the bundle to OUT.chase; otherwise it prints to stdout"
  putStrLn "  -a points at the annotations JSON (default: .inference/chase-unison-annotations.json"
  putStrLn "     in the project this is run from)"
  putStrLn "     keyed by a single module named after <project>"
  exitFailure

runList :: String -> String -> String -> Maybe T.Text -> IO ()
runList url project branch ns = do
  cb <- newCodebase url (T.pack project) (T.pack branch)
  listNamespace cb ns Nothing >>= \case
    Left err -> TIO.putStrLn ("ERROR: " <> err) >> exitFailure
    Right l  -> do
      TIO.putStrLn (listFqn' l <> "  " <> T.take 12 (listHash l))
      mapM_ printChild (listChildren l)
  where
    listFqn' l = let f = listFqn l in if T.null f then "(root)" else f

runExtract :: String -> String -> String -> T.Text -> Maybe FilePath -> Maybe FilePath -> IO ()
runExtract url project branch root mOut mAnn = do
  let projectT = T.pack project
  annPath <- maybe defaultAnnPath (pure . Just) mAnn
  annMap  <- case annPath of
    Nothing -> pure Map.empty
    Just p  ->
      loadAnnotationsIfExists p >>= \case
        Right m  -> pure m
        Left err -> do
          hPutStrLn stderr ("chase-unison: " <> err)
          exitFailure
  let anns = maybe (emptyAnnotations projectT) id (Map.lookup projectT annMap)
  cb <- newCodebase url projectT (T.pack branch)
  collectSkeleton cb root >>= \case
    Left err  -> TIO.putStrLn ("ERROR: " <> err) >> exitFailure
    Right usk -> do
      let res = renderAnnotated usk anns
      mapM_ (\w -> hPutStrLn stderr ("chase-unison: DRIFT " <> T.unpack w)) (arDriftWarnings res)
      case mOut of
        Nothing  -> TIO.putStr (arText res)
        Just out -> do
          TIO.writeFile out (arText res)
          hPutStrLn stderr ("chase-unison: wrote " <> out)

printChild :: Child -> IO ()
printChild c = TIO.putStrLn ("  " <> label <> "   " <> T.take 12 (childHash c))
  where
    label = case childKind c of
      KTerm _ ty    -> "term  " <> childName c <> " : " <> renderSegments ty
      KType tg      -> "type  " <> childName c <> " (" <> tg <> ")"
      KNamespace sz -> "ns/   " <> childName c <> " [" <> T.pack (show sz) <> "]"