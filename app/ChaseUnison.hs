{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chase.Unison.Api
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = getArgs >>= \case
  (url : project : branch : rest) -> do
    let ns = case rest of (n : _) -> Just (T.pack n); _ -> Nothing
    cb <- newCodebase url (T.pack project) (T.pack branch)
    listNamespace cb ns Nothing >>= \case
      Left err -> TIO.putStrLn ("ERROR: " <> err) >> exitFailure
      Right l  -> do
        TIO.putStrLn (listFqn' l <> "  " <> T.take 12 (listHash l))
        mapM_ printChild (listChildren l)
  _ -> do
    putStrLn "usage: chase-unison <baseUrl> <project> <branch> [namespace]"
    putStrLn "  baseUrl is scheme://host:port/token, e.g. http://127.0.0.1:5858/codebase"
    exitFailure
  where
    listFqn' l = let f = listFqn l in if T.null f then "(root)" else f

printChild :: Child -> IO ()
printChild c = TIO.putStrLn ("  " <> label <> "   " <> T.take 12 (childHash c))
  where
    label = case childKind c of
      KTerm _ ty    -> "term  " <> childName c <> " : " <> renderSegments ty
      KType tg      -> "type  " <> childName c <> " (" <> tg <> ")"
      KNamespace sz -> "ns/   " <> childName c <> " [" <> T.pack (show sz) <> "]"