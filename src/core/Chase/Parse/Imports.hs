{-# LANGUAGE DerivingStrategies #-}

-- | What a source file imports, and how a name in it was written, as far
-- as resolving references needs: the language-neutral shape both parsers
-- produce for "Chase.Coverage".
module Chase.Parse.Imports
  ( Ref (..)
  , ImportInfo (..)
  , ImportNames (..)
  ) where

import Data.Text (Text)

-- | A name as written in an expression: @Map.insert@ is
-- @Ref (Just "Map") "insert"@, @insert@ is @Ref Nothing "insert"@.
data Ref = Ref
  { refQualifier :: Maybe Text
  , refName      :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | One import declaration.
data ImportInfo = ImportInfo
  { impModule        :: Text
  , impAlias         :: Maybe Text
  -- ^ the @as@ name, which replaces the module name as a qualifier
  , impQualifiedOnly :: Bool
  -- ^ names are reachable only qualified: Haskell's @import qualified@,
  -- and every PureScript import with @as@
  , impNames         :: ImportNames
  }
  deriving stock (Eq, Show)

-- | Which unqualified or qualified names an import lets through.
data ImportNames
  = ImportAll
  -- ^ no list, or a list with a @T(..)@ whose members are not known here
  | ImportOnly [Text]
  | ImportHiding [Text]
  deriving stock (Eq, Show)
