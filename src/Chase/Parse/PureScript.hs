{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chase.Parse.PureScript
  ( parseSourceFile
  , scanTestBindings
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Control.Exception  (catch, SomeException)
import           Data.List          (nub)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe         (mapMaybe)

import qualified Chase.Vendor.PureScript.CST.Lexer  as PSLexer
import qualified Chase.Vendor.PureScript.CST.Parser as PSParser
import qualified Chase.Vendor.PureScript.CST.Types  as CST
import qualified Chase.Vendor.PureScript.CST.Errors as CSTErr
import qualified Chase.Vendor.PureScript.Names      as N

import           Chase.Types


parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  result <- parseCstFile path
  pure $ case result of
    Left pf        -> Left pf
    Right (src, m) -> Right (extractStructure path src m)

scanTestBindings
  :: FilePath
  -> IO (Either ParseFailure (Text, [(Text, Int, [Text])]))
scanTestBindings path = do
  result <- parseCstFile path
  pure $ case result of
    Left pf -> Left pf
    Right (_, m) ->
      Right (moduleName m, extractTestBindings m)


parseCstFile
  :: FilePath -> IO (Either ParseFailure (Text, CST.Module ()))
parseCstFile path = do
  readResult <- (Right <$> TIO.readFile path) `catch` \e ->
    pure $ Left (T.pack (show (e :: SomeException)))
  case readResult of
    Left err -> pure $ Left $ ParseFailure path ("read failed: " <> err)
    Right src ->
      case PSParser.parseModule (PSLexer.lexModule src) of
        Left errs ->
          pure $ Left $ ParseFailure path (renderParserErrors errs)
        Right partial ->
          case snd (PSParser.resFull partial) of
            Left errs ->
              pure $ Left $ ParseFailure path (renderParserErrors errs)
            Right fullModule ->
              pure $ Right (src, fullModule)

renderParserErrors :: NonEmpty CSTErr.ParserError -> Text
renderParserErrors errs =
  T.intercalate "\n  " $ map (T.pack . CSTErr.prettyPrintError) (NE.toList errs)


extractStructure :: FilePath -> Text -> CST.Module () -> ChaseFile
extractStructure path src cstModule =
  let lns         = T.lines src
      totalLines  = length lns
      decls       = CST.modDecls cstModule
      ranges      = declRanges decls totalLines
      imps        = map extractImport (CST.modImports cstModule)
      sigs        = mapMaybe (extractSignature lns) ranges
                 <> concatMap (extractClassMethods lns) ranges
      foreigns    = mapMaybe (extractForeignImport lns) ranges
      datas       = mapMaybe (extractDataDecl lns) ranges
      fixities    = mapMaybe (extractFixity lns) ranges
  in (emptyChaseFile LangPureScript path)
       { chaseModuleName     = moduleName cstModule
       , chaseImports        = imps
       , chaseSignatures     = sigs
       , chaseForeignImports = foreigns
       , chaseDataDecls      = datas
       , chaseFixities       = fixities
       }


extractTestBindings :: CST.Module () -> [(Text, Int, [Text])]
extractTestBindings cstModule =
  mapMaybe valueBindingRefs (CST.modDecls cstModule)

valueBindingRefs :: CST.Declaration () -> Maybe (Text, Int, [Text])
valueBindingRefs = \case
  CST.DeclValue _ vbf ->
    let fnName = identText (CST.nameValue (CST.valName vbf))
        line   = tokenLine (CST.nameTok (CST.valName vbf))
        refs   = nub (collectValueBindingRefs vbf)
    in Just (fnName, line, filter (/= fnName) refs)
  _ -> Nothing


extractImport :: CST.ImportDecl () -> Text
extractImport idecl =
  let m = N.runModuleName (CST.nameValue (CST.impModule idecl))
      q = case CST.impQual idecl of
            Just (_, qmod) -> " as " <> N.runModuleName (CST.nameValue qmod)
            Nothing        -> ""
  in m <> q

extractSignature
  :: [Text] -> (CST.Declaration (), Int, Int) -> Maybe Signature
extractSignature lns (decl, startL, endL) = case decl of
  CST.DeclSignature _ (CST.Labeled (CST.Name _ ident) _ _) ->
    let verbatim = sliceLines lns startL endL
    in Just (Signature (identText ident) verbatim startL)
  _ -> Nothing

extractClassMethods
  :: [Text] -> (CST.Declaration (), Int, Int) -> [Signature]
extractClassMethods lns (decl, _, _) = case decl of
  CST.DeclClass _ _ (Just (_, methods)) ->
    map (toMethodSig lns) (NE.toList methods)
  _ -> []

toMethodSig
  :: [Text]
  -> CST.Labeled (CST.Name CST.Ident) (CST.Type ())
  -> Signature
toMethodSig lns (CST.Labeled (CST.Name tok ident) _ _) =
  let line     = tokenLine tok
      verbatim = T.stripStart (sliceFirstLine lns line)
  in Signature (identText ident) verbatim line

extractForeignImport
  :: [Text] -> (CST.Declaration (), Int, Int) -> Maybe Signature
extractForeignImport lns (decl, startL, endL) = case decl of
  CST.DeclForeign _ _ _ fimp ->
    let verbatim = sliceLines lns startL endL
        name = case fimp of
          CST.ForeignValue (CST.Labeled (CST.Name _ ident) _ _) ->
            identText ident
          CST.ForeignData _ (CST.Labeled (CST.Name _ pname) _ _) ->
            N.runProperName pname
          CST.ForeignKind _ (CST.Name _ pname) ->
            N.runProperName pname
    in Just (Signature name verbatim startL)
  _ -> Nothing

extractDataDecl
  :: [Text] -> (CST.Declaration (), Int, Int) -> Maybe ChaseDataDecl
extractDataDecl lns (decl, startL, endL) = case decl of
  CST.DeclData _ hd _ ->
    Just (ChaseDataDecl
            (N.runProperName (CST.nameValue (CST.dataHdName hd)))
            (sliceLines lns startL endL)
            startL)
  CST.DeclType _ hd _ _ ->
    Just (ChaseDataDecl
            (N.runProperName (CST.nameValue (CST.dataHdName hd)))
            (sliceLines lns startL endL)
            startL)
  CST.DeclNewtype _ hd _ _ _ ->
    Just (ChaseDataDecl
            (N.runProperName (CST.nameValue (CST.dataHdName hd)))
            (sliceLines lns startL endL)
            startL)
  CST.DeclClass _ hd _ ->
    Just (ChaseDataDecl
            (N.runProperName (CST.nameValue (CST.clsName hd)))
            (sliceFirstLine lns startL)
            startL)
  CST.DeclInstanceChain _ (CST.Separated h _) ->
    let nm = N.runProperName
               (CST.qualName (CST.instClass (CST.instHead h)))
    in Just (ChaseDataDecl nm (sliceFirstLine lns startL) startL)
  CST.DeclDerive _ _ _ instHead ->
    let nm = N.runProperName (CST.qualName (CST.instClass instHead))
    in Just (ChaseDataDecl nm (sliceFirstLine lns startL) startL)
  _ -> Nothing

extractFixity
  :: [Text] -> (CST.Declaration (), Int, Int) -> Maybe Fixity
extractFixity lns (decl, startL, _) = case decl of
  CST.DeclFixity _ ff ->
    let verbatim = sliceFirstLine lns startL
        opName = case CST.fxtOp ff of
          CST.FixityValue _ _ (CST.Name _ opn) -> N.runOpName opn
          CST.FixityType _ _ _ (CST.Name _ opn) -> N.runOpName opn
    in Just (Fixity verbatim [opName] startL)
  _ -> Nothing


collectValueBindingRefs :: CST.ValueBindingFields () -> [Text]
collectValueBindingRefs vbf =
  case CST.valGuarded vbf of
    CST.Unconditional _ wheree -> collectFromWhere wheree
    CST.Guarded gexprs         -> concatMap collectFromGuardedExpr (NE.toList gexprs)

collectFromWhere :: CST.Where () -> [Text]
collectFromWhere (CST.Where expr mBindings) =
  collectFromExpr expr <>
  case mBindings of
    Nothing -> []
    Just (_, bs) -> concatMap collectFromLetBinding (NE.toList bs)

collectFromGuardedExpr :: CST.GuardedExpr () -> [Text]
collectFromGuardedExpr (CST.GuardedExpr _ pats _ wheree) =
  concatMap collectFromPatternGuard (sepToList pats)
  <> collectFromWhere wheree

collectFromPatternGuard :: CST.PatternGuard () -> [Text]
collectFromPatternGuard (CST.PatternGuard _ expr) = collectFromExpr expr

collectFromLetBinding :: CST.LetBinding () -> [Text]
collectFromLetBinding = \case
  CST.LetBindingSignature _ _ -> []
  CST.LetBindingName _ vbf    -> collectValueBindingRefs vbf
  CST.LetBindingPattern _ _ _ w -> collectFromWhere w

collectFromExpr :: CST.Expr () -> [Text]
collectFromExpr = \case
  CST.ExprHole _ _                      -> []
  CST.ExprSection _ _                   -> []
  CST.ExprIdent _ qn                    -> [identText (CST.qualName qn)]
  CST.ExprConstructor _ _               -> []
  CST.ExprBoolean _ _ _                 -> []
  CST.ExprChar _ _ _                    -> []
  CST.ExprString _ _ _                  -> []
  CST.ExprNumber _ _ _                  -> []
  CST.ExprArray _ delim                 -> concatMap collectFromExpr (delimitedToList delim)
  CST.ExprRecord _ delim                -> concatMap collectFromRecordLabeled (delimitedToList delim)
  CST.ExprParens _ (CST.Wrapped _ e _)  -> collectFromExpr e
  CST.ExprTyped _ e _ _                 -> collectFromExpr e
  CST.ExprInfix _ a (CST.Wrapped _ op _) b ->
    collectFromExpr a <> collectFromExpr op <> collectFromExpr b
  CST.ExprOp _ a _ b                    -> collectFromExpr a <> collectFromExpr b
  CST.ExprOpName _ _                    -> []
  CST.ExprNegate _ _ e                  -> collectFromExpr e
  CST.ExprRecordAccessor _ ra           -> collectFromExpr (CST.recExpr ra)
  CST.ExprRecordUpdate _ e updates      ->
    collectFromExpr e
    <> concatMap collectFromRecordUpdate (delimitedNonEmptyToList updates)
  CST.ExprApp _ f x                     -> collectFromExpr f <> collectFromExpr x
  CST.ExprVisibleTypeApp _ e _ _        -> collectFromExpr e
  CST.ExprLambda _ lam                  -> collectFromExpr (CST.lmbBody lam)
  CST.ExprIf _ ite                      ->
    collectFromExpr (CST.iteCond ite)
    <> collectFromExpr (CST.iteTrue ite)
    <> collectFromExpr (CST.iteFalse ite)
  CST.ExprCase _ co                     ->
    concatMap collectFromExpr (sepToList (CST.caseHead co))
    <> concatMap (collectFromGuarded . snd) (NE.toList (CST.caseBranches co))
  CST.ExprLet _ li                      ->
    concatMap collectFromLetBinding (NE.toList (CST.letBindings li))
    <> collectFromExpr (CST.letBody li)
  CST.ExprDo _ block                    ->
    concatMap collectFromDoStatement (NE.toList (CST.doStatements block))
  CST.ExprAdo _ block                   ->
    concatMap collectFromDoStatement (CST.adoStatements block)
    <> collectFromExpr (CST.adoResult block)

collectFromRecordLabeled :: CST.RecordLabeled (CST.Expr ()) -> [Text]
collectFromRecordLabeled = \case
  CST.RecordPun (CST.Name _ ident) -> [identText ident]
  CST.RecordField _ _ e            -> collectFromExpr e

collectFromRecordUpdate :: CST.RecordUpdate () -> [Text]
collectFromRecordUpdate = \case
  CST.RecordUpdateLeaf _ _ e         -> collectFromExpr e
  CST.RecordUpdateBranch _ updates   ->
    concatMap collectFromRecordUpdate (delimitedNonEmptyToList updates)

collectFromGuarded :: CST.Guarded () -> [Text]
collectFromGuarded = \case
  CST.Unconditional _ wheree -> collectFromWhere wheree
  CST.Guarded gexprs         -> concatMap collectFromGuardedExpr (NE.toList gexprs)

collectFromDoStatement :: CST.DoStatement () -> [Text]
collectFromDoStatement = \case
  CST.DoLet _ bindings -> concatMap collectFromLetBinding (NE.toList bindings)
  CST.DoDiscard e      -> collectFromExpr e
  CST.DoBind _ _ e     -> collectFromExpr e


delimitedToList :: CST.Delimited a -> [a]
delimitedToList (CST.Wrapped _ Nothing _)    = []
delimitedToList (CST.Wrapped _ (Just sep) _) = sepToList sep

delimitedNonEmptyToList :: CST.DelimitedNonEmpty a -> [a]
delimitedNonEmptyToList (CST.Wrapped _ sep _) = sepToList sep

sepToList :: CST.Separated a -> [a]
sepToList (CST.Separated hd tl) = hd : map snd tl


declRanges
  :: [CST.Declaration ()]
  -> Int
  -> [(CST.Declaration (), Int, Int)]
declRanges decls totalLines = go decls
  where
    go []     = []
    go [d]    = [(d, declStartLine d, totalLines)]
    go (d : rest@(d2 : _)) =
      let s1 = declStartLine d
          s2 = declStartLine d2
      in (d, s1, max s1 (s2 - 1)) : go rest

declStartLine :: CST.Declaration () -> Int
declStartLine = \case
  CST.DeclData _ hd _              -> tokenLine (CST.dataHdKeyword hd)
  CST.DeclType _ hd _ _            -> tokenLine (CST.dataHdKeyword hd)
  CST.DeclNewtype _ hd _ _ _       -> tokenLine (CST.dataHdKeyword hd)
  CST.DeclClass _ hd _             -> tokenLine (CST.clsKeyword hd)
  CST.DeclInstanceChain _ (CST.Separated h _) ->
    tokenLine (CST.instKeyword (CST.instHead h))
  CST.DeclDerive _ tok _ _         -> tokenLine tok
  CST.DeclKindSignature _ tok _    -> tokenLine tok
  CST.DeclSignature _ (CST.Labeled (CST.Name tok _) _ _) -> tokenLine tok
  CST.DeclValue _ vbf              -> tokenLine (CST.nameTok (CST.valName vbf))
  CST.DeclFixity _ ff              -> tokenLine (fst (CST.fxtKeyword ff))
  CST.DeclForeign _ tok _ _        -> tokenLine tok
  CST.DeclRole _ tok _ _ _         -> tokenLine tok


sliceLines :: [Text] -> Int -> Int -> Text
sliceLines lns startL endL =
  let n        = endL - startL + 1
      offset   = max 0 (startL - 1)
      relevant = take n (drop offset lns)
      noTrail  = reverse (dropWhile (T.null . T.strip) (reverse relevant))
  in T.intercalate "\n" noTrail

sliceFirstLine :: [Text] -> Int -> Text
sliceFirstLine lns startL =
  case drop (max 0 (startL - 1)) lns of
    (l:_) -> l
    []    -> ""


moduleName :: CST.Module a -> Text
moduleName m = N.runModuleName (CST.nameValue (CST.modNamespace m))

identText :: CST.Ident -> Text
identText = CST.getIdent

tokenLine :: CST.SourceToken -> Int
tokenLine tok = CST.srcLine (CST.srcStart (CST.tokRange (CST.tokAnn tok)))