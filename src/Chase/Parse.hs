{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Chase.Parse
  ( parseSourceFile
  , ParseFailure (..)
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as BS

-- ghc-lib-parser
import GHC.Data.FastString       (mkFastString)
import GHC.Data.StringBuffer     (StringBuffer, stringToStringBuffer)
import qualified GHC.Settings    as Settings
import GHC.Settings.Config       (cProjectVersion)
import GHC.Platform              (genericPlatform, platformArchOS)
import GHC.Driver.Session        (DynFlags, defaultDynFlags, xopt_set, supportedLanguagesAndExtensions)
import GHC.Fingerprint           (fingerprint0)
import GHC.Unit.Types            (stringToUnitId)
import GHC.Driver.Config.Parser  (initParserOpts)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser                (parseModule)
import GHC.Parser.Header         (getOptions)
import GHC.Parser.Lexer          (ParseResult (..), PState, initParserState, unP, getPsErrorMessages)
import GHC.Types.SrcLoc          (mkRealSrcLoc, unLoc, getLoc, SrcSpan (..),
                                  srcSpanStartLine, srcSpanEndLine)
import GHC.Hs                    (HsModule (..), GhcPs)
import GHC.Hs.Decls              (HsDecl (..), TyClDecl (..), InstDecl (..),
                                  ClsInstDecl (..), FamilyDecl (..), LHsDecl)
import GHC.Hs.Binds              (Sig (..), LSig)
import GHC.Parser.Annotation     (locA)
import GHC.Types.Name.Reader     (RdrName, rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import GHC.Utils.Outputable      (ppr, showSDocUnsafe)

import Chase.Types

------------------------------------------------------------------
-- Public API
------------------------------------------------------------------

data ParseFailure = ParseFailure
  { pfPath :: FilePath
  , pfMsg  :: Text
  }

-- | Parse a Haskell source file using GHC's own parser via
-- ghc-lib-parser. Source-level LANGUAGE pragmas are read by GHC's
-- own pragma extractor and applied to DynFlags before parsing, so
-- syntactic extensions like TemplateHaskell, QuasiQuotes, and
-- TypeApplications work exactly as GHC handles them.
parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  bytes <- BS.readFile path
  let sourceText  = TE.decodeUtf8 bytes
      sourceLines = T.lines sourceText
      buf         = stringToStringBuffer (T.unpack sourceText)
      dflags      = applySourcePragmas path buf (defaultDynFlags probedSettings)
      pOpts       = initParserOpts dflags
      startLoc    = mkRealSrcLoc (mkFastString path) 1 1
      pstate0     = initParserState pOpts buf startLoc
  case unP parseModule pstate0 of
    POk _ locModule ->
      pure $ Right $ extractStructure path sourceText sourceLines (unLoc locModule)
    PFailed pst ->
      pure $ Left $ ParseFailure path (renderParseError pst)

------------------------------------------------------------------
-- LANGUAGE pragma application
------------------------------------------------------------------

-- | Read LANGUAGE pragmas from the source buffer and turn each one
-- into a DynFlags update. This is what GHC's own driver does before
-- handing a file to the parser; we replicate it for the parser-only
-- call path. Without this, files using TemplateHaskell, QuasiQuotes,
-- TypeApplications, etc. fail to parse because their syntactic
-- productions are gated behind the corresponding extension flag.
applySourcePragmas :: FilePath -> StringBuffer -> DynFlags -> DynFlags
applySourcePragmas path buf df0 =
  let initialOpts       = initParserOpts df0
      (_warns, locOpts) = getOptions initialOpts (supportedLanguagesAndExtensions (platformArchOS genericPlatform)) buf path
      flagStrings       = map unLoc locOpts
  in foldl' applyOne df0 flagStrings
  where
    applyOne df flag =
      case parseExtensionFlag flag of
        Just LangExt.TemplateHaskell ->
          -- TemplateHaskell implies TemplateHaskellQuotes, which is what
          -- gates the `$` lexer rule. xopt_set does NOT walk extension
          -- implications, so we apply this one explicitly.
          xopt_set (xopt_set df LangExt.TemplateHaskell)
                   LangExt.TemplateHaskellQuotes
        Just ext -> xopt_set df ext
        Nothing  -> df

-- | Parse a "-XFoo" or "-XNoFoo" string into an Extension. We do
-- not handle "No*" prefixes because chase only adds extensions; if
-- a file disables a default-on extension that disabling will be
-- silently ignored, which is fine for parsing purposes (disabling
-- an extension never enables new syntax that would otherwise fail).
parseExtensionFlag :: String -> Maybe LangExt.Extension
parseExtensionFlag s = case s of
  '-':'X':rest -> readExtension rest
  _            -> Nothing
  where
    readExtension name =
      case [ ext | ext <- [minBound .. maxBound :: LangExt.Extension]
                 , show ext == name ] of
        (e:_) -> Just e
        []    -> Nothing

------------------------------------------------------------------
-- AST extraction
------------------------------------------------------------------

extractStructure
  :: FilePath -> Text -> [Text] -> HsModule GhcPs -> ChaseFile
extractStructure path sourceText sourceLines hsmod =
  (emptyChaseFile path)
    { chaseModuleName = moduleNameOf hsmod
    , chaseExtensions = pragmasFromSource sourceText
    , chaseImports    = importsOf hsmod
    , chaseSignatures = concatMap (extractSignatures sourceLines) (hsmodDecls hsmod)
    , chaseDataDecls  = concatMap (extractDataDecl  sourceLines) (hsmodDecls hsmod)
    }

moduleNameOf :: HsModule GhcPs -> Text
moduleNameOf m = case hsmodName m of
  Just lmn -> T.pack (moduleNameString (unLoc lmn))
  Nothing  -> ""

-- | Pretty-print parse errors from a failed PState. Uses GHC's
-- own ppr instance so the format matches what ghc itself prints.
renderParseError :: PState -> Text
renderParseError pst =
  let msgs = getPsErrorMessages pst
  in T.pack (showSDocUnsafe (ppr msgs))

importsOf :: HsModule GhcPs -> [Text]
importsOf m = map renderImport (hsmodImports m)
  where
    renderImport limp = T.pack (showSDocUnsafe (ppr limp))

------------------------------------------------------------------
-- Signatures (top-level + class methods)
------------------------------------------------------------------

extractSignatures :: [Text] -> LHsDecl GhcPs -> [Signature]
extractSignatures sourceLines ldecl =
  let sp = locA (getLoc ldecl)
  in case unLoc ldecl of
       SigD _ sig -> sigToSignatures sourceLines sp sig
       TyClD _ (ClassDecl { tcdSigs = sigs }) ->
         concatMap (extractClassSig sourceLines) sigs
       _ -> []

extractClassSig :: [Text] -> LSig GhcPs -> [Signature]
extractClassSig sourceLines lsig =
  let sp = locA (getLoc lsig)
  in sigToSignatures sourceLines sp (unLoc lsig)

sigToSignatures :: [Text] -> SrcSpan -> Sig GhcPs -> [Signature]
sigToSignatures sourceLines sp = \case
  TypeSig _ names _ ->
    [ Signature (rdrNameToText (unLoc n))
                (sliceSourceSpan sourceLines sp)
                (startLine sp)
    | n <- names ]
  ClassOpSig _ _ names _ ->
    [ Signature (rdrNameToText (unLoc n))
                (sliceSourceSpan sourceLines sp)
                (startLine sp)
    | n <- names ]
  _ -> []

------------------------------------------------------------------
-- Data, newtype, type, class, instance, type/data family
------------------------------------------------------------------

extractDataDecl :: [Text] -> LHsDecl GhcPs -> [ChaseDataDecl]
extractDataDecl sourceLines ldecl =
  let sp = locA (getLoc ldecl)
  in case unLoc ldecl of
       TyClD _ tcd -> case tcd of
         DataDecl  { tcdLName = n } -> [mkFull sp n]
         SynDecl   { tcdLName = n } -> [mkFull sp n]
         ClassDecl { tcdLName = n } -> [mkHeadOnly sp n]
         FamDecl _ (FamilyDecl { fdLName = n }) -> [mkFull sp n]
       InstD _ (ClsInstD _ (ClsInstDecl { cid_poly_ty = lty })) ->
         [ ChaseDataDecl
             (T.pack (showSDocUnsafe (ppr lty)))
             (sliceFirstLine sourceLines sp)
             (startLine sp)
         ]
       _ -> []
  where
    mkFull sp lname = ChaseDataDecl
      (rdrNameToText (unLoc lname))
      (sliceSourceSpan sourceLines sp)
      (startLine sp)
    mkHeadOnly sp lname = ChaseDataDecl
      (rdrNameToText (unLoc lname))
      (sliceFirstLine sourceLines sp)
      (startLine sp)

------------------------------------------------------------------
-- Names and source-span slicing
------------------------------------------------------------------

rdrNameToText :: RdrName -> Text
rdrNameToText = T.pack . occNameString . rdrNameOcc

startLine :: SrcSpan -> Int
startLine = \case
  RealSrcSpan rss _ -> srcSpanStartLine rss
  UnhelpfulSpan _   -> 0

sliceSourceSpan :: [Text] -> SrcSpan -> Text
sliceSourceSpan ls = \case
  RealSrcSpan rss _ ->
    let s = srcSpanStartLine rss
        e = srcSpanEndLine   rss
    in T.intercalate "\n" $ take (e - s + 1) $ drop (s - 1) ls
  UnhelpfulSpan _ -> ""

sliceFirstLine :: [Text] -> SrcSpan -> Text
sliceFirstLine ls = \case
  RealSrcSpan rss _ ->
    let s = srcSpanStartLine rss
    in case drop (s - 1) ls of
         (line:_) -> line
         []       -> ""
  UnhelpfulSpan _ -> ""

------------------------------------------------------------------
-- LANGUAGE pragma extraction (for chase output, separate from
-- the DynFlags update above)
------------------------------------------------------------------

pragmasFromSource :: Text -> [Text]
pragmasFromSource src =
  [ T.strip ext
  | line <- takeWhile isPreambleLine (T.lines src)
  , Just rest <- [T.stripPrefix "{-#" (T.strip line)]
  , Just exts <- [extractLanguagePragma rest]
  , ext <- T.splitOn "," exts
  ]
  where
    isPreambleLine line =
      let s = T.strip line
      in T.null s
      || "--" `T.isPrefixOf` s
      || "{-" `T.isPrefixOf` s
      || "#"  `T.isPrefixOf` s
    extractLanguagePragma rest =
      let s = T.strip rest
      in case T.stripPrefix "LANGUAGE" s of
           Just body ->
             let body' = T.strip body
                 stripped = case T.stripSuffix "#-}" body' of
                   Just b  -> T.strip b
                   Nothing -> body'
             in Just stripped
           Nothing -> Nothing

------------------------------------------------------------------
-- DynFlags / Settings placeholder construction
------------------------------------------------------------------

probedSettings :: Settings.Settings
probedSettings = Settings.Settings
  { Settings.sGhcNameVersion = probedGhcNameVersion
  , Settings.sFileSettings   = probedFileSettings
  , Settings.sTargetPlatform = genericPlatform
  , Settings.sToolSettings   = probedToolSettings
  , Settings.sPlatformMisc   = probedPlatformMisc
  , Settings.sRawSettings    = []
  , Settings.sUnitSettings   = probedUnitSettings
  }

probedGhcNameVersion :: Settings.GhcNameVersion
probedGhcNameVersion = Settings.GhcNameVersion
  { Settings.ghcNameVersion_programName    = "ghc"
  , Settings.ghcNameVersion_projectVersion = cProjectVersion
  }

probedFileSettings :: Settings.FileSettings
probedFileSettings = Settings.FileSettings
  { Settings.fileSettings_ghcUsagePath          = ""
  , Settings.fileSettings_ghciUsagePath         = ""
  , Settings.fileSettings_topDir                = ""
  , Settings.fileSettings_toolDir               = Nothing
  , Settings.fileSettings_globalPackageDatabase = ""
  }

probedPlatformMisc :: Settings.PlatformMisc
probedPlatformMisc = Settings.PlatformMisc
  { Settings.platformMisc_targetPlatformString               = ""
  , Settings.platformMisc_ghcWithInterpreter                 = False
  , Settings.platformMisc_libFFI                             = False
  , Settings.platformMisc_llvmTarget                         = ""
  , Settings.platformMisc_targetRTSLinkerOnlySupportsSharedLibs = False
  }

probedUnitSettings :: Settings.UnitSettings
probedUnitSettings = Settings.UnitSettings
  { Settings.unitSettings_baseUnitId = stringToUnitId "base"
  }

probedToolSettings :: Settings.ToolSettings
probedToolSettings = Settings.ToolSettings
  { Settings.toolSettings_ldSupportsCompactUnwind        = False
  , Settings.toolSettings_ldSupportsFilelist             = False
  , Settings.toolSettings_ldSupportsSingleModule         = False
  , Settings.toolSettings_mergeObjsSupportsResponseFiles = False
  , Settings.toolSettings_ldIsGnuLd                      = False
  , Settings.toolSettings_ccSupportsNoPie                = False
  , Settings.toolSettings_useInplaceMinGW                = False
  , Settings.toolSettings_arSupportsDashL                = False
  , Settings.toolSettings_cmmCppSupportsG0               = False
  , Settings.toolSettings_pgm_L                          = ""
  , Settings.toolSettings_pgm_P                          = ("", [])
  , Settings.toolSettings_pgm_JSP                        = ("", [])
  , Settings.toolSettings_pgm_CmmP                       = ("", [])
  , Settings.toolSettings_pgm_F                          = ""
  , Settings.toolSettings_pgm_c                          = ""
  , Settings.toolSettings_pgm_cxx                        = ""
  , Settings.toolSettings_pgm_cpp                        = ("", [])
  , Settings.toolSettings_pgm_a                          = ("", [])
  , Settings.toolSettings_pgm_l                          = ("", [])
  , Settings.toolSettings_pgm_lm                         = Nothing
  , Settings.toolSettings_pgm_windres                    = ""
  , Settings.toolSettings_pgm_ar                         = ""
  , Settings.toolSettings_pgm_otool                      = ""
  , Settings.toolSettings_pgm_install_name_tool          = ""
  , Settings.toolSettings_pgm_ranlib                     = ""
  , Settings.toolSettings_pgm_lo                         = ("", [])
  , Settings.toolSettings_pgm_lc                         = ("", [])
  , Settings.toolSettings_pgm_las                        = ("", [])
  , Settings.toolSettings_pgm_i                          = ""
  , Settings.toolSettings_opt_L                          = []
  , Settings.toolSettings_opt_P                          = []
  , Settings.toolSettings_opt_JSP                        = []
  , Settings.toolSettings_opt_CmmP                       = []
  , Settings.toolSettings_opt_P_fingerprint              = fingerprint0
  , Settings.toolSettings_opt_JSP_fingerprint            = fingerprint0
  , Settings.toolSettings_opt_CmmP_fingerprint           = fingerprint0
  , Settings.toolSettings_opt_F                          = []
  , Settings.toolSettings_opt_c                          = []
  , Settings.toolSettings_opt_cxx                        = []
  , Settings.toolSettings_opt_a                          = []
  , Settings.toolSettings_opt_l                          = []
  , Settings.toolSettings_opt_lm                         = []
  , Settings.toolSettings_opt_windres                    = []
  , Settings.toolSettings_opt_lo                         = []
  , Settings.toolSettings_opt_lc                         = []
  , Settings.toolSettings_opt_las                        = []
  , Settings.toolSettings_opt_i                          = []
  , Settings.toolSettings_extraGccViaCFlags              = []
  }