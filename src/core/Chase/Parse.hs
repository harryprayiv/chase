{-# LANGUAGE OverloadedStrings #-}

module Chase.Parse
  ( parseSourceFile
  , parseSourceFileFull
  , ParseFailure (..)
  ) where

import qualified Data.Text       as T
import           System.FilePath (takeExtension)

import           Chase.Derive           (Parsed (..))
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

-- | The skeleton plus the derive pass. PureScript has no derive pass yet:
-- its files report no sum types and derive nothing, so they take part in
-- the run without contributing edges, clauses or callers.
parseSourceFileFull :: FilePath -> IO (Either ParseFailure Parsed)
parseSourceFileFull path = case takeExtension path of
  ".hs"   -> HS.parseSourceFileFull path
  ".purs" -> fmap plain <$> PS.parseSourceFile path
  ext     -> pure $ Left $ ParseFailure path
               ("unsupported source extension: " <> T.pack ext)
  where
    plain cf = Parsed
      { parsedFile     = cf
      , parsedSumTypes = []
      , parsedDerive   = const emptyDerived
      }