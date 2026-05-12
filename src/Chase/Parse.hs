{-# LANGUAGE OverloadedStrings #-}

module Chase.Parse
  ( parseSourceFile
  , ParseFailure (..)
  ) where

import qualified Data.Text       as T
import           System.FilePath (takeExtension)

import           Chase.Types
import qualified Chase.Parse.Haskell    as HS
import qualified Chase.Parse.PureScript as PS

-- | Dispatch on file extension. Adding a new backend is a one-line addition
-- here plus a new Chase.Parse.<Lang> module.
parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = case takeExtension path of
  ".hs"   -> HS.parseSourceFile path
  ".purs" -> PS.parseSourceFile path
  ext     -> pure $ Left $ ParseFailure path
               ("unsupported source extension: " <> T.pack ext)