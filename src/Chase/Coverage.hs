{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chase.Coverage
  ( TestBinding (..)
  , TestRefIndex
  , scanTestFile
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
import GHC.Types.Name.Reader     (RdrName, rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)

import           Chase.Types
import qualified Chase.Parse.Haskell    as HSP
import qualified Chase.Parse.PureScript as PSP

-- | One top-level function binding in a test module, with the set of
-- variable names that appear inside its body.
data TestBinding = TestBinding
  { tbModule   :: Text
  , tbFunction :: Text
  , tbLine     :: Int
  , tbRefs     :: [Text]
  }
  deriving Show

type TestRefIndex = Map Text [TestRef]


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
    Right (modName, entries) -> Right
      [ TestBinding modName fn ln refs
      | (fn, ln, refs) <- entries
      ]

-- | Pure: extract TestBindings from an already-parsed Haskell module.
scanTestFile :: Text -> HsModule GhcPs -> [TestBinding]
scanTestFile modName hsmod =
  concatMap (fromDecl modName) (hsmodDecls hsmod)

fromDecl :: Text -> LHsDecl GhcPs -> [TestBinding]
fromDecl modName ldecl =
  let sp = locA (getLoc ldecl)
      ln = startLine sp
  in case unLoc ldecl of
       ValD _ (FunBind { fun_id = lname, fun_matches = mg }) ->
         [ TestBinding modName
                       (rdrNameToText (unLoc lname))
                       ln
                       (nub (collectVarsIn mg))
         ]
       _ -> []

-- | Invert TestBindings into a name -> [TestRef] index. Self-references
-- (a binding mentioning its own name, e.g. recursive helpers) are
-- dropped. Within an index entry, refs are deduplicated by (module,
-- function) and sorted by source line.
buildTestIndex :: [TestBinding] -> TestRefIndex
buildTestIndex bindings = Map.fromListWith mergeRefs raw
  where
    raw =
      [ (refName, [TestRef (tbModule tb) (tbFunction tb) (tbLine tb)])
      | tb <- bindings
      , refName <- tbRefs tb
      , refName /= tbFunction tb
      ]
    mergeRefs a b = sortOn trSrcLine $ nubBy sameRef (a <> b)
    sameRef x y = trModule x == trModule y
               && trFunction x == trFunction y

-- | Attach coverage info to a ChaseFile by looking up each signature,
-- foreign import, and pattern name in the index. The result is always
-- Just (never Nothing), so the renderer emits a ? line for every
-- entry (either "tested by: ..." or "no test references").
attachCoverage :: TestRefIndex -> ChaseFile -> ChaseFile
attachCoverage idx cf = cf
  { chaseTestCoverage = Just $
       [ (sigName s, look (sigName s)) | s <- chaseSignatures     cf ]
    <> [ (sigName s, look (sigName s)) | s <- chaseForeignImports cf ]
    <> [ (patName p, look (patName p)) | p <- chasePatterns       cf ]
  }
  where
    look k = Map.findWithDefault [] k idx


-- | SYB walk: find every HsVar occurrence in any subterm. Reaches
-- inside let, case, do, lambda, application, sections, record
-- construction, all of it. Haskell-only.
collectVarsIn :: Data a => a -> [Text]
collectVarsIn = SYB.everything (++) (SYB.mkQ [] visit)
  where
    visit :: HsExpr GhcPs -> [Text]
    visit (HsVar _ ln) = [rdrNameToText (unLoc ln)]
    visit _            = []

rdrNameToText :: RdrName -> Text
rdrNameToText = T.pack . occNameString . rdrNameOcc

startLine :: SrcSpan -> Int
startLine = \case
  RealSrcSpan rss _ -> srcSpanStartLine rss
  UnhelpfulSpan _   -> 0