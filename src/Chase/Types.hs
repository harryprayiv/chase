{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Chase.Types
  ( ChaseFile (..)
  , emptyChaseFile
  , SourceLanguage (..)
  , ParseFailure (..)
  , Signature (..)
  , Pattern (..)
  , Fixity (..)
  , ChaseDataDecl (..)
  , ModuleAnnotations (..)
  , emptyAnnotations
  , Constant (..)
  , Note (..)
  , Invariant (..)
  , inv
  , invHinted
  , constNote
  , constBug
  , Decision (..)
  , decision
  , OpenIssue (..)
  , openIssue
  , Topology (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Which language a parsed file is in. Used by the renderer to suppress
-- sections that are not meaningful for one language (e.g. %ext for PureScript)
-- and to allow the pipeline to dispatch to the correct parser.
data SourceLanguage = LangHaskell | LangPureScript
  deriving stock (Show, Eq, Generic)

-- | Parser-level failure. Lifted to Chase.Types so that both the Haskell and
-- PureScript backends can produce it without depending on each other.
data ParseFailure = ParseFailure
  { pfPath :: FilePath
  , pfMsg  :: Text
  }
  deriving stock (Show, Generic)

data ChaseFile = ChaseFile
  { chaseSourcePath     :: FilePath
  , chaseLanguage       :: SourceLanguage
  , chaseModuleName     :: Text
  , chaseExtensions     :: [Text]          -- Haskell only; empty for PureScript
  , chaseFixities       :: [Fixity]
  , chaseImports        :: [Text]
  , chaseSignatures     :: [Signature]
  , chaseForeignImports :: [Signature]     -- PureScript only; empty for Haskell
  , chasePatterns       :: [Pattern]       -- Haskell only; empty for PureScript
  , chaseDataDecls      :: [ChaseDataDecl]
  , chaseConstants      :: [Constant]
  , chaseInvariants     :: [Invariant]
  , chaseDecisions      :: [Decision]
  , chaseOpenIssues     :: [OpenIssue]
  , chaseTopologies     :: [Topology]
  , chaseParseErrors    :: [Text]
  }
  deriving stock (Show, Generic)

emptyChaseFile :: SourceLanguage -> FilePath -> ChaseFile
emptyChaseFile lang path = ChaseFile
  { chaseSourcePath     = path
  , chaseLanguage       = lang
  , chaseModuleName     = ""
  , chaseExtensions     = []
  , chaseFixities       = []
  , chaseImports        = []
  , chaseSignatures     = []
  , chaseForeignImports = []
  , chasePatterns       = []
  , chaseDataDecls      = []
  , chaseConstants      = []
  , chaseInvariants     = []
  , chaseDecisions      = []
  , chaseOpenIssues     = []
  , chaseTopologies     = []
  , chaseParseErrors    = []
  }

data Signature = Signature
  { sigName     :: Text
  , sigVerbatim :: Text
  , sigSrcLine  :: Int
  }
  deriving stock (Show, Generic)

data Pattern = Pattern
  { patName     :: Text
  , patVerbatim :: Text
  , patSrcLine  :: Int
  }
  deriving stock (Show, Generic)

data Fixity = Fixity
  { fixVerbatim :: Text
  , fixOps      :: [Text]
  , fixSrcLine  :: Int
  }
  deriving stock (Show, Generic)

data ChaseDataDecl = ChaseDataDecl
  { cddName     :: Text
  , cddVerbatim :: Text
  , cddSrcLine  :: Int
  }
  deriving stock (Show, Generic)

data ModuleAnnotations = ModuleAnnotations
  { annModName    :: Text
  , annConstants  :: [Constant]
  , annInvariants :: [Invariant]
  , annDecisions  :: [Decision]
  , annOpenIssues :: [OpenIssue]
  , annTopologies :: [Topology]
  }
  deriving stock (Show, Generic)

emptyAnnotations :: Text -> ModuleAnnotations
emptyAnnotations m = ModuleAnnotations m [] [] [] [] []

data Constant = Constant
  { constName     :: Text
  , constValueDoc :: Text
  , constNotes    :: [Note]
  }
  deriving stock (Show, Generic)

data Note = Note Text | BugNote Text
  deriving stock (Show, Generic)

constNote :: Text -> Note
constNote = Note

constBug :: Text -> Note
constBug = BugNote

data Invariant = Invariant
  { invFunction :: Text
  , invLines    :: [Text]
  , invConsumes :: [Text]
  , invBodyHint :: Maybe Text
  }
  deriving stock (Show, Generic)

inv :: Text -> [Text] -> Invariant
inv fn ls = Invariant fn ls [] Nothing

invHinted :: Text -> [Text] -> Text -> Invariant
invHinted fn ls hint = Invariant fn ls [] (Just hint)

data Decision = Decision
  { decName    :: Text
  , decWhat    :: Text
  , decWhy     :: Text
  , decAffects :: [Text]
  }
  deriving stock (Show, Generic)

decision :: Text -> Text -> Text -> [Text] -> Decision
decision = Decision

data OpenIssue = OpenIssue
  { oiName     :: Text
  , oiWhat     :: Text
  , oiWhy      :: Text
  , oiBlocking :: [Text]
  , oiAffects  :: [Text]
  }
  deriving stock (Show, Generic)

openIssue :: Text -> Text -> Text -> [Text] -> [Text] -> OpenIssue
openIssue = OpenIssue

data Topology = Topology
  { topName        :: Text
  , topVertices    :: [Text]
  , topTransitions :: [(Text, [Text])]
  }
  deriving stock (Show, Generic)