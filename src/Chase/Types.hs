{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Chase.Types
  ( ChaseFile (..)
  , emptyChaseFile
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
  , Topology (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | The canonical structural description of one Haskell source file as
-- chase sees it. Structural fields are populated by Chase.Parse;
-- annotation fields (constants, invariants, decisions, topologies) are
-- merged in later by Chase.Pipeline.attachAnnotations.
data ChaseFile = ChaseFile
  { chaseSourcePath  :: FilePath
  , chaseModuleName  :: Text
  , chaseExtensions  :: [Text]
  , chaseFixities    :: [Fixity]
  , chaseImports     :: [Text]
  , chaseSignatures  :: [Signature]
  , chasePatterns    :: [Pattern]
  , chaseDataDecls   :: [ChaseDataDecl]
  , chaseConstants   :: [Constant]
  , chaseInvariants  :: [Invariant]
  , chaseDecisions   :: [Decision]
  , chaseTopologies  :: [Topology]
  , chaseParseErrors :: [Text]
  }
  deriving stock (Show, Generic)

emptyChaseFile :: FilePath -> ChaseFile
emptyChaseFile path = ChaseFile
  { chaseSourcePath  = path
  , chaseModuleName  = ""
  , chaseExtensions  = []
  , chaseFixities    = []
  , chaseImports     = []
  , chaseSignatures  = []
  , chasePatterns    = []
  , chaseDataDecls   = []
  , chaseConstants   = []
  , chaseInvariants  = []
  , chaseDecisions   = []
  , chaseTopologies  = []
  , chaseParseErrors = []
  }

data Signature = Signature
  { sigName     :: Text
  , sigVerbatim :: Text
  , sigSrcLine  :: Int
  }
  deriving stock (Show, Generic)

-- | A pattern synonym, possibly with a separately-declared type
-- signature. patVerbatim is the rendered combined view: type sig (if
-- present) followed by definition.
data Pattern = Pattern
  { patName     :: Text
  , patVerbatim :: Text
  , patSrcLine  :: Int
  }
  deriving stock (Show, Generic)

-- | A top-level fixity declaration, e.g. 'infixr 5 :<>'. fixVerbatim
-- preserves the source as written. fixOps are the operator names this
-- declaration applies to (one fixity decl can name multiple ops).
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
  , annTopologies :: [Topology]
  }
  deriving stock (Show, Generic)

emptyAnnotations :: Text -> ModuleAnnotations
emptyAnnotations m = ModuleAnnotations m [] [] [] []

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

data Topology = Topology
  { topName        :: Text
  , topVertices    :: [Text]
  , topTransitions :: [(Text, [Text])]
  }
  deriving stock (Show, Generic)