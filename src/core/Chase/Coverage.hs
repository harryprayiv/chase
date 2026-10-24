{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Which bindings refer to which definitions, for the ? tested-by lines
-- and for chase-annotate's callers.
--
-- A reference is resolved to the module that defines it through the
-- imports of the file it appears in, and the index is keyed by (module,
-- name). Until 2026-10-24 it was keyed by the bare name, so two modules
-- that both defined parse were each shown as tested by every test that
-- called either, and a test calling one of them qualified was indistinct
-- from one calling the other.
--
-- Resolution follows the language's import rules without knowing what each
-- module exports:
--
--   * a qualified reference Q.f resolves to every module imported as Q,
--     or imported under its own name Q with no alias (an alias replaces
--     the module's name as a qualifier, in both languages), whose import
--     list lets f through;
--   * an unqualified reference f resolves to the referring module itself
--     and to every module imported unqualified whose import list lets f
--     through.
--
-- An import with no list lets every name through, so an unqualified
-- reference in a file with two open imports resolves to both. That is an
-- over-approximation only among the modules the file actually imports,
-- which is where an unqualified name can come from.
module Chase.Coverage
  ( TestBinding (..)
  , Ref (..)
  , ImportInfo (..)
  , ImportNames (..)
  , TestRefIndex
  , scanTestFile
  , resolveRef
  , buildTestIndex
  , attachCoverage
  , parseTestFile
  ) where

import qualified Data.Generics    as SYB
import           Data.Generics    (Data)
import           Data.List        (nub, nubBy, sortOn)
import qualified Data.Map.Strict  as Map
import           Data.Map.Strict  (Map)
import qualified Data.Text        as T
import           Data.Text        (Text)
import           System.FilePath  (takeExtension)

import GHC.Hs                    (HsModule (..), GhcPs)
import GHC.Hs.Decls              (HsDecl (..), LHsDecl)
import GHC.Hs.Binds              (HsBindLR (..))
import GHC.Hs.Expr               (HsExpr (..))
import GHC.Parser.Annotation     (locA)
import GHC.Types.SrcLoc          (unLoc, getLoc, SrcSpan (..),
                                  srcSpanStartLine)
import GHC.Types.Name.Reader     (RdrName, rdrNameOcc, isQual_maybe)
import GHC.Types.Name.Occurrence (occNameString)
import Language.Haskell.Syntax.Module.Name (moduleNameString)

import           Chase.Types
import qualified Chase.Parse.Haskell    as HSP
import qualified Chase.Parse.PureScript as PSP
import           Chase.Parse.Imports    (ImportInfo (..), ImportNames (..), Ref (..))

-- | One top-level function binding, with the references inside its body
-- and the imports of the file it is in.
data TestBinding = TestBinding
  { tbModule   :: Text
  , tbFunction :: Text
  , tbLine     :: Int
  , tbRefs     :: [Ref]
  , tbImports  :: [ImportInfo]
  }
  deriving Show

-- | (defining module, name) to the bindings that refer to it.
type TestRefIndex = Map (Text, Text) [TestRef]


-- | Read a test file from disk and extract its top-level test bindings.
-- Dispatches by extension: .hs uses ghc-lib-parser via Chase.Parse.Haskell,
-- .purs uses the vendored CST via Chase.Parse.PureScript. Parse failures
-- are surfaced as Left; the caller decides whether to skip the file or
-- abort.
parseTestFile :: FilePath -> IO (Either ParseFailure [TestBinding])
parseTestFile path = case takeExtension path of
  ".purs" -> parsePureScriptTestFile path
  _       -> parseHaskellTestFile path

parseHaskellTestFile :: FilePath -> IO (Either ParseFailure [TestBinding])
parseHaskellTestFile path = do
  result <- HSP.parseHsModule path
  pure $ case result of
    Left pf          -> Left pf
    Right (_, hsmod) ->
      let modName = HSP.moduleNameOf hsmod
      in Right (scanTestFile modName hsmod)

parsePureScriptTestFile :: FilePath -> IO (Either ParseFailure [TestBinding])
parsePureScriptTestFile path = do
  result <- PSP.scanTestBindings path
  pure $ case result of
    Left pf -> Left pf
    Right (modName, imports, entries) -> Right
      [ TestBinding modName fn ln refs imports
      | (fn, ln, refs) <- entries
      ]

-- | Pure: extract TestBindings from an already-parsed Haskell module.
scanTestFile :: Text -> HsModule GhcPs -> [TestBinding]
scanTestFile modName hsmod =
  concatMap (fromDecl modName (HSP.importInfosOf hsmod)) (hsmodDecls hsmod)

fromDecl :: Text -> [ImportInfo] -> LHsDecl GhcPs -> [TestBinding]
fromDecl modName imports ldecl =
  let sp = locA (getLoc ldecl)
      ln = startLine sp
  in case unLoc ldecl of
       ValD _ (FunBind { fun_id = lname, fun_matches = mg }) ->
         [ TestBinding modName
                       (rdrNameToText (unLoc lname))
                       ln
                       (nub (collectVarsIn mg))
                       imports
         ]
       _ -> []

-- | The modules a reference in a binding of the given module can name.
resolveRef :: Text -> [ImportInfo] -> Ref -> [Text]
resolveRef self imports Ref{..} = nub $ case refQualifier of
  Just q ->
    [ impModule i
    | i <- imports
    , maybe (impModule i) id (impAlias i) == q
    , lets refName (impNames i)
    ]
  Nothing ->
    self :
    [ impModule i
    | i <- imports
    , not (impQualifiedOnly i)
    , lets refName (impNames i)
    ]
  where
    lets n = \case
      ImportAll       -> True
      ImportOnly ns   -> n `elem` ns
      ImportHiding ns -> n `notElem` ns

-- | Invert TestBindings into a (module, name) -> [TestRef] index.
-- Self-references (a binding mentioning its own name, e.g. recursive
-- helpers) are dropped. Within an index entry, refs are deduplicated by
-- (module, function) and sorted by source line.
buildTestIndex :: [TestBinding] -> TestRefIndex
buildTestIndex bindings = Map.fromListWith mergeRefs raw
  where
    raw =
      [ ((m, refName r), [TestRef (tbModule tb) (tbFunction tb) (tbLine tb)])
      | tb <- bindings
      , r <- tbRefs tb
      , m <- resolveRef (tbModule tb) (tbImports tb) r
      , not (m == tbModule tb && refName r == tbFunction tb)
      ]
    mergeRefs a b = sortOn trSrcLine $ nubBy sameRef (a <> b)
    sameRef x y = trModule x == trModule y
               && trFunction x == trFunction y

-- | Attach coverage info to a ChaseFile by looking up each signature,
-- foreign import, and pattern name, in this file's module, in the index.
-- The result is always Just (never Nothing), so the renderer emits a ?
-- line for every entry (either "tested by: ..." or "no test references").
attachCoverage :: TestRefIndex -> ChaseFile -> ChaseFile
attachCoverage idx cf = cf
  { chaseTestCoverage = Just $
       [ (sigName s, look (sigName s)) | s <- chaseSignatures     cf ]
    <> [ (sigName s, look (sigName s)) | s <- chaseForeignImports cf ]
    <> [ (patName p, look (patName p)) | p <- chasePatterns       cf ]
  }
  where
    look k = Map.findWithDefault [] (chaseModuleName cf, k) idx


-- | SYB walk: find every HsVar occurrence in any subterm, with its
-- qualifier. Reaches inside let, case, do, lambda, application, sections,
-- record construction, all of it. Haskell-only.
collectVarsIn :: Data a => a -> [Ref]
collectVarsIn = SYB.everything (++) (SYB.mkQ [] visit)
  where
    visit :: HsExpr GhcPs -> [Ref]
    visit (HsVar _ ln) = [refOf (unLoc ln)]
    visit _            = []

refOf :: RdrName -> Ref
refOf n = case isQual_maybe n of
  Just (q, occ) -> Ref (Just (T.pack (moduleNameString q))) (T.pack (occNameString occ))
  Nothing       -> Ref Nothing (rdrNameToText n)

rdrNameToText :: RdrName -> Text
rdrNameToText = T.pack . occNameString . rdrNameOcc

startLine :: SrcSpan -> Int
startLine = \case
  RealSrcSpan rss _ -> srcSpanStartLine rss
  UnhelpfulSpan _   -> 0