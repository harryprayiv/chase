module Chase.Parse
  ( parseSourceFile
  , ParseFailure (..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts
  ( ParseResult (..)
  , Module (..)
  , ModuleHead (..)
  , ModuleName (..)
  , ModulePragma (..)
  , ImportDecl (..)
  , Decl (..)
  , ClassDecl (..)
  , Name (..)
  , DeclHead (..)
  , InstHead (..)
  , InstRule (..)
  , SrcSpan (..)
  , SrcSpanInfo (..)
  , parseFileWithMode
  , defaultParseMode
  , ParseMode (..)
  , glasgowExts
  , prettyPrint
  , KnownExtension (..)
  , Extension (..)
  , Fixity (..)
  , Assoc (..)
  , QName (..)
  , baseFixities
  )

import Chase.Types

data ParseFailure = ParseFailure
  { pfPath :: FilePath
  , pfMsg  :: Text
  }

-- | Parse a Haskell source file and return its structural skeleton.
-- Returns either a ParseFailure with diagnostic, or a partial ChaseFile
-- containing only the structural fields. Annotations are merged later
-- by the pipeline.
parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  source <- TIO.readFile path
  let sourceLines = T.lines source
      mode = defaultParseMode
        { parseFilename = path
        , extensions =
               glasgowExts
            <> map EnableExtension knownExtras
            <> map UnknownExtension unknownExtras
        , fixities = Just (baseFixities <> commonOperatorFixities)
        }
  result <- parseFileWithMode mode path
  case result of
    ParseFailed loc msg ->
      pure $ Left $ ParseFailure path
        (T.pack ("at " <> show loc <> ": " <> msg))
    ParseOk parsed ->
      pure $ Right $ extractStructure path sourceLines parsed
  where
    knownExtras =
      [ DataKinds, OverloadedStrings, LambdaCase, RecordWildCards
      , BlockArguments, NamedFieldPuns, DerivingStrategies, DeriveGeneric
      , DeriveFunctor, MultiWayIf
      , TupleSections, ViewPatterns, PatternSynonyms, BangPatterns
      , StrictData, TypeApplications, PolyKinds, ConstraintKinds
      , DerivingVia, QuantifiedConstraints
      , RoleAnnotations
      ]

    unknownExtras =
      [ "NumericUnderscores"
      , "ApplicativeDo"
      , "AllowAmbiguousTypes"
      , "StandaloneKindSignatures"
      ]

-- | Fixities for operators from libraries we commonly encounter but
-- whose imports haskell-src-exts does not chase. Without these, code
-- like @x & a .~ b & c ?~ d@ produces "ambiguous infix expression".
-- Add new entries here when a parse failure traces back to an unknown
-- operator's precedence.
commonOperatorFixities :: [Fixity]
commonOperatorFixities =
  [ fixity (AssocLeft  ()) 1 "&"
  , fixity (AssocRight ()) 4 ".~"
  , fixity (AssocRight ()) 4 "?~"
  , fixity (AssocRight ()) 4 "%~"
  , fixity (AssocRight ()) 4 "+~"
  , fixity (AssocRight ()) 4 "-~"
  , fixity (AssocRight ()) 4 "<>~"
  , fixity (AssocLeft  ()) 8 "^."
  , fixity (AssocLeft  ()) 8 "^.."
  , fixity (AssocLeft  ()) 8 "^?"
  , fixity (AssocLeft  ()) 8 "^?!"
  , fixity (AssocLeft  ()) 1 "&~"
  , fixity (AssocRight ()) 9 "#."
  , fixity (AssocLeft  ()) 9 ".#"
  , fixity (AssocRight ()) 1 ":~>"
  , fixity (AssocRight ()) 1 ":~"
  , fixity (AssocRight ()) 8 ".:"
  , fixity (AssocRight ()) 8 ".:?"
  , fixity (AssocRight ()) 8 ".:!"
  , fixity (AssocRight ()) 8 ".!="
  , fixity (AssocRight ()) 8 ".="
  , fixity (AssocRight ()) 4 ":>"
  , fixity (AssocRight ()) 3 ":<|>"
  ]
  where
    fixity assoc prec sym =
      Fixity assoc prec (UnQual () (Symbol () sym))

extractStructure :: FilePath -> [Text] -> Module SrcSpanInfo -> ChaseFile
extractStructure path sourceLines = \case
  Module _ mhead pragmas imports decls ->
    (emptyChaseFile path)
      { chaseModuleName = maybe "" extractModuleName mhead
      , chaseExtensions = extractPragmas pragmas
      , chaseImports    = map extractImport imports
      , chaseSignatures = concatMap (extractSignatures sourceLines) decls
      , chaseDataDecls  = mapMaybe (extractChaseDataDecl sourceLines) decls
      }
  _ -> emptyChaseFile path

extractModuleName :: ModuleHead l -> Text
extractModuleName (ModuleHead _ (ModuleName _ n) _ _) = T.pack n

extractPragmas :: [ModulePragma SrcSpanInfo] -> [Text]
extractPragmas = concatMap \case
  LanguagePragma _ names -> map nameText names
  _                      -> []

nameText :: Name l -> Text
nameText (Ident _ n)  = T.pack n
nameText (Symbol _ n) = T.pack n

extractImport :: ImportDecl SrcSpanInfo -> Text
extractImport = T.pack . prettyPrint

-- | Extract type signatures from one top-level declaration.
--
-- Recurses into class declarations to pick up method signatures.
-- Class method signatures are emitted unqualified — the same name
-- form as top-level signatures — which means two classes declaring
-- a method with the same name (e.g. two classes each declaring
-- @run@) produce two 'Signature' values with identical 'sigName'.
-- The renderer attaches at most one invariant per sigName, so this
-- collision is currently a documented limitation rather than a fix.
extractSignatures :: [Text] -> Decl SrcSpanInfo -> [Signature]
extractSignatures sourceLines = \case
  TypeSig l names _ ->
    let line     = startLine l
        verbatim = sliceSourceSpan sourceLines l
    in [ Signature (nameText n) verbatim line | n <- names ]
  ClassDecl _ _ _ _ mBody ->
    case mBody of
      Nothing   -> []
      Just body -> concatMap (extractClassMethodSig sourceLines) body
  _ -> []

-- | Pull TypeSig declarations out of a class body. ClsDecl wraps a
-- regular Decl; we recurse via extractSignatures so default-method
-- ClsDefSig and other future sig-bearing variants would be added in
-- one place.
extractClassMethodSig :: [Text] -> ClassDecl SrcSpanInfo -> [Signature]
extractClassMethodSig sourceLines = \case
  ClsDecl _ d -> extractSignatures sourceLines d
  _           -> []

-- | Extract a verbatim type-shaped declaration: data, newtype, type
-- alias, class, instance, or type/data family.
--
-- DataDecl, GDataDecl, TypeDecl, TypeFamDecl, DataFamDecl, and
-- ClosedTypeFamDecl render their full source span verbatim because
-- the body IS the signature (constructors, fields, kind, equations).
--
-- ClassDecl and InstDecl render only the HEAD line. The body of a
-- class is method signatures already extracted by extractSignatures;
-- the body of an instance is method definitions, which chase
-- deliberately drops everywhere else. Beyond keeping the output
-- focused, head-only slicing also avoids a haskell-src-exts
-- source-span quirk where a class or instance span can over-extend
-- past the actual end of the declaration and pull a stray signature
-- from the following top-level binding into the rendered block.
extractChaseDataDecl :: [Text] -> Decl SrcSpanInfo -> Maybe ChaseDataDecl
extractChaseDataDecl sourceLines = \case
  DataDecl l _ _ dh _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  GDataDecl l _ _ dh _ _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  TypeDecl l dh _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  ClassDecl l _ dh _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceFirstLine sourceLines l) (startLine l)
  InstDecl l _ irule _ ->
    Just $ ChaseDataDecl (instRuleName irule) (sliceFirstLine sourceLines l) (startLine l)
  TypeFamDecl l dh _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  DataFamDecl l _ dh _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  ClosedTypeFamDecl l dh _ _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  _ -> Nothing

declHeadName :: DeclHead l -> Text
declHeadName = \case
  DHead _ n        -> nameText n
  DHInfix _ _ n    -> nameText n
  DHParen _ inner  -> declHeadName inner
  DHApp _ inner _  -> declHeadName inner

-- | Extract a textual identifier from an instance rule.
--
-- Instances do not have a single name in the language sense; this
-- returns the head class name (e.g. @Show@ for @instance Show Foo@)
-- so the rendered output and any drift-check messages have something
-- printable.
instRuleName :: InstRule SrcSpanInfo -> Text
instRuleName = \case
  IRule _ _ _ ihead   -> instHeadName ihead
  IParen _ inner      -> instRuleName inner

instHeadName :: InstHead SrcSpanInfo -> Text
instHeadName = \case
  IHCon _ qn       -> qNameText qn
  IHInfix _ _ qn   -> qNameText qn
  IHParen _ inner  -> instHeadName inner
  IHApp _ inner _  -> instHeadName inner

qNameText :: QName l -> Text
qNameText = \case
  Qual _ (ModuleName _ m) n -> T.pack m <> "." <> nameText n
  UnQual _ n                -> nameText n
  Special _ _               -> "<special>"

startLine :: SrcSpanInfo -> Int
startLine si = srcSpanStartLine (srcInfoSpan si)

-- | Slice all lines covered by a source span, 1-indexed.
sliceSourceSpan :: [Text] -> SrcSpanInfo -> Text
sliceSourceSpan ls si =
  let sp = srcInfoSpan si
      s  = srcSpanStartLine sp
      e  = srcSpanEndLine sp
  in T.intercalate "\n" $ take (e - s + 1) $ drop (s - 1) ls

-- | Slice ONLY the starting line of a source span. Used for class
-- and instance declarations where rendering the body is undesirable
-- (class bodies are method sigs, extracted separately; instance
-- bodies are method defs, deliberately dropped) AND where the
-- haskell-src-exts span can over-extend past the actual end of the
-- declaration into the following top-level binding.
sliceFirstLine :: [Text] -> SrcSpanInfo -> Text
sliceFirstLine ls si =
  let sp = srcInfoSpan si
      s  = srcSpanStartLine sp
  in case drop (s - 1) ls of
       (line:_) -> line
       []       -> ""