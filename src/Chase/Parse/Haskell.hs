{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Chase.Parse.Haskell
  ( parseSourceFile
  , parseHsModule
  , moduleNameOf
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as BS
import           Data.List          (sortOn)
import qualified Data.Map.Strict    as Map

import GHC.Data.FastString       (mkFastString)
import GHC.Data.StringBuffer     (StringBuffer, stringToStringBuffer)
import qualified GHC.Settings    as Settings
import GHC.Settings.Config       (cProjectVersion)
import GHC.Platform              (genericPlatform)
import GHC.Driver.Session        (DynFlags, defaultDynFlags, xopt_set)
import GHC.Fingerprint           (fingerprint0)
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
import GHC.Hs.Binds              (Sig (..), LSig, HsBindLR (..))
import GHC.Parser.Annotation     (locA)
import GHC.Types.Name.Reader     (RdrName, rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import GHC.Utils.Outputable      (ppr, showSDocUnsafe)

import Chase.Types

-- | Raw parse: returns the source text and the AST so other passes
-- (Chase.Coverage) can walk it without re-parsing.
parseHsModule :: FilePath -> IO (Either ParseFailure (Text, HsModule GhcPs))
parseHsModule path = do
  bytes <- BS.readFile path
  let sourceText  = TE.decodeUtf8 bytes
      buf         = stringToStringBuffer (T.unpack sourceText)
      dflags      = applySourcePragmas path buf (defaultDynFlags probedSettings)
      pOpts       = initParserOpts dflags
      startLoc    = mkRealSrcLoc (mkFastString path) 1 1
      pstate0     = initParserState pOpts buf startLoc
  case unP parseModule pstate0 of
    POk _ locModule ->
      pure $ Right (sourceText, unLoc locModule)
    PFailed pst ->
      pure $ Left $ ParseFailure path (renderParseError pst)

parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  result <- parseHsModule path
  pure $ case result of
    Left pf -> Left pf
    Right (sourceText, hsmod) ->
      Right $ extractStructure path sourceText (T.lines sourceText) hsmod


applySourcePragmas :: FilePath -> StringBuffer -> DynFlags -> DynFlags
applySourcePragmas path buf df0 =
  let initialOpts       = initParserOpts df0
      (_warns, locOpts) = getOptions initialOpts buf path
      flagStrings       = map unLoc locOpts
  in foldl' applyOne df0 flagStrings
  where
    applyOne df flag =
      case parseExtensionFlag flag of
        Just LangExt.TemplateHaskell ->
          xopt_set (xopt_set df LangExt.TemplateHaskell)
                   LangExt.TemplateHaskellQuotes
        Just ext -> xopt_set df ext
        Nothing  -> df

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


extractStructure
  :: FilePath -> Text -> [Text] -> HsModule GhcPs -> ChaseFile
extractStructure path sourceText sourceLines hsmod =
  (emptyChaseFile LangHaskell path)
    { chaseModuleName = moduleNameOf hsmod
    , chaseExtensions = pragmasFromSource sourceText
    , chaseFixities   = concatMap (extractFixity sourceLines) (hsmodDecls hsmod)
    , chaseImports    = importsOf hsmod
    , chaseSignatures = concatMap (extractSignatures sourceLines) (hsmodDecls hsmod)
    , chasePatterns   = mergePatterns
                          (concatMap (extractPatternSig  sourceLines) (hsmodDecls hsmod))
                          (concatMap (extractPatternBind sourceLines) (hsmodDecls hsmod))
    , chaseDataDecls  = concatMap (extractDataDecl  sourceLines) (hsmodDecls hsmod)
    }

renderParseError :: PState -> Text
renderParseError pst =
  let msgs = getPsErrorMessages pst
  in T.pack (showSDocUnsafe (ppr msgs))

moduleNameOf :: HsModule GhcPs -> Text
moduleNameOf m = case hsmodName m of
  Just lmn -> T.pack (moduleNameString (unLoc lmn))
  Nothing  -> ""

importsOf :: HsModule GhcPs -> [Text]
importsOf m = map renderImport (hsmodImports m)
  where
    renderImport limp = T.pack (showSDocUnsafe (ppr limp))


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


extractPatternSig :: [Text] -> LHsDecl GhcPs -> [(Text, Int, Text)]
extractPatternSig sourceLines ldecl =
  let sp = locA (getLoc ldecl)
  in case unLoc ldecl of
       SigD _ (PatSynSig _ names _) ->
         [ ( rdrNameToText (unLoc n)
           , startLine sp
           , sliceSourceSpan sourceLines sp
           )
         | n <- names ]
       _ -> []

extractPatternBind :: [Text] -> LHsDecl GhcPs -> [(Text, Int, Text)]
extractPatternBind sourceLines ldecl =
  let sp = locA (getLoc ldecl)
  in case unLoc ldecl of
       ValD _ bind -> case bind of
         PatSynBind _ _ ->
           let rendered = sliceSourceSpan sourceLines sp
               nm       = patternNameFromSource rendered
           in case nm of
                Just n  -> [(n, startLine sp, rendered)]
                Nothing -> []
         _ -> []
       _ -> []

patternNameFromSource :: Text -> Maybe Text
patternNameFromSource src =
  let trimmed = T.dropWhile isHSpace src
  in case T.stripPrefix "pattern" trimmed of
       Nothing -> Nothing
       Just rest ->
         let body = T.dropWhile isHSpace rest
         in case T.uncons body of
              Just ('(', afterOpen) ->
                let opName = T.takeWhile (/= ')') afterOpen
                in if T.null opName then Nothing else Just (T.strip opName)
              _ ->
                let firstTok = T.takeWhile (\c -> not (isHSpace c)
                                                 && c /= '\n'
                                                 && c /= ',') body
                    rest'    = T.dropWhile isHSpace
                                 (T.dropWhile (\c -> not (isHSpace c)) body)
                    nextTok  = T.takeWhile (\c -> not (isHSpace c)
                                                 && c /= '\n') rest'
                in if isOperatorish nextTok && not (T.null nextTok)
                     then Just nextTok
                     else if T.null firstTok
                       then Nothing
                       else Just firstTok
  where
    isHSpace c = c == ' ' || c == '\t'
    isOperatorish t =
         not (T.null t)
      && T.all isOpChar t
      && t `notElem` reservedSyntax
    isOpChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])
    reservedSyntax = ["=", "<-", "::", "->", "|", "@"]

mergePatterns
  :: [(Text, Int, Text)]
  -> [(Text, Int, Text)]
  -> [Pattern]
mergePatterns sigs binds =
  let sigMap  = Map.fromList [ (n, (line, txt)) | (n, line, txt) <- sigs ]
      bindMap = Map.fromList [ (n, (line, txt)) | (n, line, txt) <- binds ]
      allNames = Map.keys (Map.union sigMap bindMap)
      build name =
        let mSig  = Map.lookup name sigMap
            mBind = Map.lookup name bindMap
            line = case (mSig, mBind) of
              (Just (l, _), _)       -> l
              (Nothing, Just (l, _)) -> l
              _                      -> 0
            verbatim = case (mSig, mBind) of
              (Just (_, s), Just (_, b)) -> s <> "\n" <> b
              (Just (_, s), Nothing)     -> s
              (Nothing, Just (_, b))     -> b
              _                          -> ""
        in Pattern name verbatim line
  in sortOn patSrcLine (map build allNames)


extractFixity :: [Text] -> LHsDecl GhcPs -> [Fixity]
extractFixity sourceLines ldecl =
  let sp = locA (getLoc ldecl)
  in case unLoc ldecl of
       SigD _ (FixSig _ _) ->
         let verbatim = sliceSourceSpan sourceLines sp
         in [ Fixity verbatim (fixityOpsFromSource verbatim) (startLine sp) ]
       _ -> []

fixityOpsFromSource :: Text -> [Text]
fixityOpsFromSource src =
  let trimmed = T.strip src
      afterKw = case asum [ T.stripPrefix kw trimmed | kw <- ["infixr", "infixl", "infix"] ] of
        Just rest -> T.stripStart rest
        Nothing   -> trimmed
      afterPrec = T.dropWhile (\c -> c == ' ' || (c >= '0' && c <= '9'))
                              (T.dropWhile (== ' ') afterKw)
      ops = T.splitOn "," afterPrec
  in filter (not . T.null) (map T.strip ops)
  where
    asum []     = Nothing
    asum (x:xs) = case x of
      Just _  -> x
      Nothing -> asum xs


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


probedSettings :: Settings.Settings
probedSettings = Settings.Settings
  { Settings.sGhcNameVersion = probedGhcNameVersion
  , Settings.sFileSettings   = probedFileSettings
  , Settings.sTargetPlatform = genericPlatform
  , Settings.sToolSettings   = probedToolSettings
  , Settings.sPlatformMisc   = probedPlatformMisc
  , Settings.sRawSettings    = []
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