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
  , OpenIssue (..)
  , openIssue
  , Topology (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | The canonical structural description of one Haskell source file as
-- chase sees it. Structural fields are populated by Chase.Parse;
-- annotation fields (constants, invariants, decisions, openIssues,
-- topologies) are merged in later by
-- Chase.Pipeline.attachAnnotations.
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
  , chaseOpenIssues  :: [OpenIssue]
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
  , chaseOpenIssues  = []
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

-- | A known problem in the module: something that does not yet work
-- right, or works but with a caveat the reader needs to know about.
-- Distinct from 'Decision' (settled tradeoff) by the addition of
-- 'oiBlocking', a free-text list of what the issue is blocking
-- downstream (services, features, executables, callers).
--
-- 'oiAffects' is drift-checked against same-file signatures and
-- patterns, the same way 'decAffects' is. 'oiBlocking' is NOT
-- drift-checked because it commonly names downstream concepts that
-- aren't functions in this file.
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