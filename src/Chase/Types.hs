module Chase.Types
  ( -- * Module-level chase record
    ChaseFile (..)
  , emptyChaseFile
    -- * Structural pieces extracted from source
  , Signature (..)
  , ChaseDataDecl (..)
    -- * Hand-curated annotations
  , ModuleAnnotations (..)
  , emptyAnnotations
  , Constant (..)
  , Note (..)
  , Invariant (..)
  , Decision (..)
  , Topology (..)
    -- * Annotation helpers
  , inv
  , invHinted
  , constNote
  , constBug
  , decision
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | The full extracted plus annotated representation of one source file.
data ChaseFile = ChaseFile
  { chaseSourcePath  :: FilePath
  , chaseModuleName  :: Text
  , chaseExtensions  :: [Text]
  , chaseImports     :: [Text]
  , chaseSignatures  :: [Signature]
  , chaseDataDecls   :: [ChaseDataDecl]
  , chaseConstants   :: [Constant]
  , chaseInvariants  :: [Invariant]
  , chaseDecisions   :: [Decision]
  , chaseTopologies  :: [Topology]
  , chaseParseErrors :: [Text]
  }
  deriving stock (Show, Generic)

emptyChaseFile :: FilePath -> ChaseFile
emptyChaseFile p = ChaseFile
  { chaseSourcePath  = p
  , chaseModuleName  = ""
  , chaseExtensions  = []
  , chaseImports     = []
  , chaseSignatures  = []
  , chaseDataDecls   = []
  , chaseConstants   = []
  , chaseInvariants  = []
  , chaseDecisions   = []
  , chaseTopologies  = []
  , chaseParseErrors = []
  }

-- | A type signature, captured verbatim from source by source span slice.
data Signature = Signature
  { sigName     :: Text
  , sigVerbatim :: Text
  , sigSrcLine  :: Int
  }
  deriving stock (Show, Generic)

-- | A data, newtype, or type alias declaration, captured verbatim.
data ChaseDataDecl = ChaseDataDecl
  { cddName     :: Text
  , cddVerbatim :: Text
  , cddSrcLine  :: Int
  }
  deriving stock (Show, Generic)

-- | All hand-curated annotations for a single module.
data ModuleAnnotations = ModuleAnnotations
  { annModName    :: Text
  , annConstants  :: [Constant]
  , annInvariants :: [Invariant]
  , annDecisions  :: [Decision]
  , annTopologies :: [Topology]
  }
  deriving stock (Show, Generic)

emptyAnnotations :: Text -> ModuleAnnotations
emptyAnnotations name = ModuleAnnotations
  { annModName    = name
  , annConstants  = []
  , annInvariants = []
  , annDecisions  = []
  , annTopologies = []
  }

-- | A named constant with optional notes. Useful for hoisting magic numbers.
data Constant = Constant
  { constName     :: Text
  , constValueDoc :: Text
  , constNotes    :: [Note]
  }
  deriving stock (Show, Generic)

-- | A note attached to a constant. 'BugNote' renders with a BUG: prefix.
data Note = Note Text | BugNote Text
  deriving stock (Show, Generic)

-- | One or more invariant lines attached to a function by name.
--
-- 'invConsumes' lists downstream code that depends on the function or
-- its observable side effects. Renders as "  > consumed by: ..." lines
-- after the "!" invariant lines. These are documentation pointers, not
-- callgraph references — entries are free text and may include module
-- qualification, descriptive parentheticals, etc.
--
-- 'invBodyHint' is the optional escape-hatch line like
-- @"body: ~30 lines, see source"@.
data Invariant = Invariant
  { invFunction :: Text
  , invLines    :: [Text]
  , invConsumes :: [Text]
  , invBodyHint :: Maybe Text
  }
  deriving stock (Show, Generic)

-- | A first-class architectural decision, attached to one or more functions.
data Decision = Decision
  { decName    :: Text
  , decWhat    :: Text
  , decWhy     :: Text
  , decAffects :: [Text]
  }
  deriving stock (Show, Generic)

-- | A state machine topology. Renders as %topology lines in the output.
data Topology = Topology
  { topName        :: Text
  , topVertices    :: [Text]
  , topTransitions :: [(Text, [Text])]
  }
  deriving stock (Show, Generic)

-- helpers -----------------------------------------------------------

inv :: Text -> [Text] -> Invariant
inv n ls = Invariant n ls [] Nothing

invHinted :: Text -> [Text] -> Text -> Invariant
invHinted n ls h = Invariant n ls [] (Just h)

constNote :: Text -> Note
constNote = Note

constBug :: Text -> Note
constBug = BugNote

decision :: Text -> Text -> Text -> [Text] -> Decision
decision = Decision