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
  , Name (..)
  , DeclHead (..)
  , SrcSpan (..)
  , SrcSpanInfo (..)
  , parseFileWithMode
  , defaultParseMode
  , ParseMode (..)
  , glasgowExts
  , prettyPrint
  , KnownExtension (..)
  , Extension (..)
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
        }
  result <- parseFileWithMode mode path
  case result of
    ParseFailed loc msg ->
      pure $ Left $ ParseFailure path
        (T.pack ("at " <> show loc <> ": " <> msg))
    ParseOk parsed ->
      pure $ Right $ extractStructure path sourceLines parsed
  where
    -- Extensions known to this version of haskell-src-exts.
    knownExtras =
      [ DataKinds, OverloadedStrings, LambdaCase, RecordWildCards
      , BlockArguments, NamedFieldPuns, DerivingStrategies, DeriveGeneric
      , DeriveFunctor, MultiWayIf
      , TupleSections, ViewPatterns, PatternSynonyms, BangPatterns
      , StrictData, TypeApplications, PolyKinds, ConstraintKinds
      , DerivingVia, QuantifiedConstraints
      , RoleAnnotations
      ]

    -- Extensions newer than this haskell-src-exts release knows about.
    -- Stored as UnknownExtension strings; the parser will accept the
    -- pragma but may not implement the syntax. Files using these
    -- features may still fail to parse and will be reported as
    -- ParseFailures by the pipeline.
    unknownExtras =
      [ "NumericUnderscores"
      , "ApplicativeDo"
      , "AllowAmbiguousTypes"
      , "StandaloneKindSignatures"
      ]

extractStructure :: FilePath -> [Text] -> Module SrcSpanInfo -> ChaseFile
extractStructure path sourceLines = \case
  Module _ mhead pragmas imports decls ->
    (emptyChaseFile path)
      { chaseModuleName = maybe "" extractModuleName mhead
      , chaseExtensions = extractPragmas pragmas
      , chaseImports    = map extractImport imports
      , chaseSignatures = concatMap (extractSignature sourceLines) decls
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

extractSignature :: [Text] -> Decl SrcSpanInfo -> [Signature]
extractSignature sourceLines = \case
  TypeSig l names _ ->
    let line     = startLine l
        verbatim = sliceSourceSpan sourceLines l
    in [ Signature (nameText n) verbatim line | n <- names ]
  _ -> []

extractChaseDataDecl :: [Text] -> Decl SrcSpanInfo -> Maybe ChaseDataDecl
extractChaseDataDecl sourceLines = \case
  DataDecl l _ _ dh _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  GDataDecl l _ _ dh _ _ _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  TypeDecl l dh _ ->
    Just $ ChaseDataDecl (declHeadName dh) (sliceSourceSpan sourceLines l) (startLine l)
  _ -> Nothing

declHeadName :: DeclHead l -> Text
declHeadName = \case
  DHead _ n        -> nameText n
  DHInfix _ _ n    -> nameText n
  DHParen _ inner  -> declHeadName inner
  DHApp _ inner _  -> declHeadName inner

startLine :: SrcSpanInfo -> Int
startLine si = srcSpanStartLine (srcInfoSpan si)

sliceSourceSpan :: [Text] -> SrcSpanInfo -> Text
sliceSourceSpan ls si =
  let sp = srcInfoSpan si
      s  = srcSpanStartLine sp
      e  = srcSpanEndLine sp
  in T.intercalate "\n" $ take (e - s + 1) $ drop (s - 1) ls