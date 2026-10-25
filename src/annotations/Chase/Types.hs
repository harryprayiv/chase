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
  , TestRef (..)
  , Derived (..)
  , emptyDerived
  , DerivedEdge (..)
  , DerivedTopology (..)
  , AbstractType (..)
  , HieFacts (..)
  , ConstructorUsage (..)
  , GhcWarning (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

data SourceLanguage = LangHaskell | LangPureScript
  deriving stock (Show, Eq, Generic)

data ParseFailure = ParseFailure
  { pfPath :: FilePath
  , pfMsg  :: Text
  }
  deriving stock (Show, Generic)

data ChaseFile = ChaseFile
  { chaseSourcePath     :: FilePath
  , chaseLanguage       :: SourceLanguage
  , chaseModuleName     :: Text
  , chaseExtensions     :: [Text]
  , chaseFixities       :: [Fixity]
  , chaseImports        :: [Text]
  , chaseSignatures     :: [Signature]
  , chaseForeignImports :: [Signature]
  , chasePatterns       :: [Pattern]
  , chaseDataDecls      :: [ChaseDataDecl]
  , chaseConstants      :: [Constant]
  , chaseInvariants     :: [Invariant]
  , chaseDecisions      :: [Decision]
  , chaseOpenIssues     :: [OpenIssue]
  , chaseTopologies     :: [Topology]
  , chaseParseErrors    :: [Text]
  , chaseTestCoverage   :: Maybe [(Text, [TestRef])]
  , chaseCallers        :: Maybe [(Text, [TestRef])]
  -- ^ callers of each signature, foreign import and pattern; from the
  -- HIE files when present, else from the run's own source roots by
  -- import resolution; Nothing when the derive pass is off
  , chaseDerived        :: Maybe Derived
  -- ^ facts read from the parsed bodies; Nothing when the derive pass is
  -- off or the language backend has no derive pass yet
  , chaseHie            :: Maybe HieFacts
  -- ^ facts from the module's HIE file; Nothing when there is none
  , chaseGhcWarnings    :: [GhcWarning]
  -- ^ the compiler's warnings for this file, from a build log
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
  , chaseTestCoverage   = Nothing
  , chaseCallers        = Nothing
  , chaseDerived        = Nothing
  , chaseHie            = Nothing
  , chaseGhcWarnings    = []
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

-- | A hand-written state machine. 'topTerminal' lists the vertices that
-- are allowed to have no outbound edge; a vertex outside that list with
-- no outbound edge is reported by the derive pass. An empty list means
-- the author has not said which states are terminal, and that check is
-- skipped for the topology.
data Topology = Topology
  { topName        :: Text
  , topVertices    :: [Text]
  , topTransitions :: [(Text, [Text])]
  , topTerminal    :: [Text]
  }
  deriving stock (Show, Generic)

-- | One reference to a target function from a test source file.
-- trModule   : name of the test module (e.g. \"Test.Auth\")
-- trFunction : top-level test binding that contains the reference
-- trSrcLine  : 1-indexed line of the test binding head
data TestRef = TestRef
  { trModule   :: Text
  , trFunction :: Text
  , trSrcLine  :: Int
  }
  deriving stock (Show, Generic)

-- | Facts the derive pass reads from one module's parsed bodies. Every
-- entry is keyed by the top-level function it was found in. These are
-- may-facts: a clause may be unreachable, a literal may be harmless, an
-- edge may never fire. The renderer marks them with a ~ sigil so a reader
-- can tell them from hand-written invariants.
data Derived = Derived
  { derClauses    :: [(Text, [Text])]
  -- ^ per function: one line per clause with a constructor or literal
  -- pattern or a guard, as "pats | guard, guard"
  , derFails      :: [(Text, [Text])]
  -- ^ per function: error constructors and error strings it can raise
  , derLiterals   :: [(Text, [Text])]
  -- ^ per function: literals in the body other than 0, 1 and ""
  , derConstCands :: [(Text, Text)]
  -- ^ top-level zero-argument bindings whose body is a literal
  , derEdges      :: [DerivedEdge]
  -- ^ transitions between constructors of a sum type, found in clauses
  -- and case alternatives
  , derAbstract   :: [AbstractType]
  -- ^ data types exported without their constructors
  }
  deriving stock (Show, Generic)

emptyDerived :: Derived
emptyDerived = Derived [] [] [] [] [] []

-- | A clause or case alternative that matches 'deFrom' and builds 'deTo',
-- both constructors of 'deType'. 'deFrom' is "*" when the clause matches
-- no constructor of that type. 'deWitness' is "Module.function".
data DerivedEdge = DerivedEdge
  { deType    :: Text
  , deFrom    :: Text
  , deTo      :: Text
  , deWitness :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | All derived edges of one sum type across the whole run, with the
-- type's constructors so a hand-written topology can be matched to it.
data DerivedTopology = DerivedTopology
  { dtType         :: Text
  , dtModule       :: Text
  , dtConstructors :: [Text]
  , dtEdges        :: [DerivedEdge]
  }
  deriving stock (Show, Generic)

-- | A data type whose constructors are not all exported. 'atHidden' are
-- the constructors the export list keeps in; 'atMakers' are the exported
-- functions of the same module whose result type names the type.
data AbstractType = AbstractType
  { atName   :: Text
  , atHidden :: [Text]
  , atMakers :: [Text]
  }
  deriving stock (Show, Generic)

-- | Facts from one module's HIE file. 'hfUses' is, per binding, the
-- functions it references that are defined outside the project's
-- packages and whose type at the use site is effectful, as
-- "Module.name". 'hfUnreferenced' lists bindings that are not exported
-- and that no binding in any HIE file references.
data HieFacts = HieFacts
  { hfUses         :: [(Text, [Text])]
  , hfUnreferenced :: [Text]
  }
  deriving stock (Show, Generic)

-- | A constructor nothing builds, or a constructor of a multi-constructor
-- type nothing matches, with the bindings on the other side as
-- "Module.function".
data ConstructorUsage = ConstructorUsage
  { cuModule   :: Text
  , cuType     :: Text
  , cuName     :: Text
  , cuBuilders :: [Text]
  , cuMatchers :: [Text]
  }
  deriving stock (Show, Generic)

-- | One warning from a GHC build log: the file and line it points at,
-- the -W flag that produced it, its message on one line, and the
-- signature it was attached to (the nearest one at or above its line),
-- or Nothing for a warning that precedes every signature of the file.
data GhcWarning = GhcWarning
  { gwFile     :: Text
  , gwLine     :: Int
  , gwFlag     :: Text
  , gwMessage  :: Text
  , gwFunction :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)