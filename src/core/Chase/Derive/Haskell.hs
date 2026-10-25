{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The derive pass over a parsed Haskell module.
--
-- Everything here reads the GhcPs tree that Chase.Parse.Haskell already
-- holds, before the bodies are dropped. No renaming and no type checking
-- happen, so every fact is syntactic and may be wider than the truth:
-- a clause may be unreachable, a constructor built in a branch may never
-- be returned, a literal may be a list index. The facts are still
-- deterministic and still ones a reader of the signature alone cannot
-- know.
--
-- Constructor shapes that changed between ghc-lib-parser releases are
-- matched by record field or by showConstr rather than by position, so a
-- release bump either compiles or fails at the usual Settings record,
-- not silently here. The extension constructors (XConDecl, XMatch and
-- the rest) are uninhabited at GhcPs, so they get no alternative.
module Chase.Derive.Haskell
  ( sumTypesOf
  , deriveHaskell
  ) where

import           Data.Char                 (isAlphaNum)
import           Data.Foldable             (toList)
import qualified Data.Generics             as SYB
import           Data.Generics             (Data)
import           Data.List                 (nub)
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Data.Text                 (Text)

import GHC.Data.FastString       (unpackFS)
import GHC.Hs                    (HsModule (..), GhcPs)
import GHC.Hs.Binds              (HsBindLR (..))
import GHC.Hs.Decls              (HsDecl (..), TyClDecl (..), ConDecl (..))
import GHC.Hs.Expr               (HsExpr (..), LHsExpr, MatchGroup (..),
                                  Match (..), GRHSs (..), GRHS (..), LGRHS)
import GHC.Hs.ImpExp             (ieNames)
import GHC.Hs.Lit                (HsLit (..), HsOverLit (..), OverLitVal (..))
import GHC.Hs.Pat                (Pat (..), LPat)
import GHC.Types.Name.Occurrence (isDataOcc, occNameString)
import GHC.Types.Name.Reader     (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc          (unLoc)
import GHC.Utils.Outputable      (Outputable, ppr, showSDocUnsafe)

import Chase.Derive              (ConMap)
import Chase.Types

-- | Every data declaration of the module with two or more constructors.
sumTypesOf :: HsModule GhcPs -> [(Text, [Text])]
sumTypesOf m =
  [ (t, cons)
  | (t, cons) <- dataTypesOf m
  , length cons >= 2
  ]

-- | Every data and newtype declaration with its constructors.
dataTypesOf :: HsModule GhcPs -> [(Text, [Text])]
dataTypesOf m =
  [ (rdrText (unLoc n), constructorsOf tcd)
  | d <- hsmodDecls m
  , TyClD _ tcd@(DataDecl { tcdLName = n }) <- [unLoc d]
  ]

constructorsOf :: TyClDecl GhcPs -> [Text]
constructorsOf = nub . SYB.everything (++) (SYB.mkQ [] one)
  where
    one :: ConDecl GhcPs -> [Text]
    one = \case
      ConDeclH98  { con_name  = n  } -> [rdrText (unLoc n)]
      ConDeclGADT { con_names = ns } -> map (rdrText . unLoc) (toList ns)

-- | The derived facts of one module. The signatures are the ones the
-- skeleton already extracted; they decide which functions get lines and
-- which exported functions count as makers of an abstract type.
deriveHaskell :: Text -> [Signature] -> HsModule GhcPs -> ConMap -> Derived
deriveHaskell modName sigs m conMap =
  let binds   = topFunBinds m
      named   = Set.fromList (map sigName sigs)
      withSig = [ b | b@(n, _) <- binds, Set.member n named ]
  in Derived
    { derClauses    = keep [ (n, concatMap clauseLines (matchesOf mg)) | (n, mg) <- withSig ]
    , derFails      = keep [ (n, failsIn mg)    | (n, mg) <- withSig ]
    , derLiterals   = keep [ (n, capped 6 (literalsIn mg)) | (n, mg) <- withSig ]
    , derConstCands = [ (n, v) | (n, mg) <- binds, Just v <- [constCandidate mg] ]
    , derEdges      = concat [ edgesIn conMap (modName <> "." <> n) mg | (n, mg) <- binds ]
    , derAbstract   = abstractTypesOf m sigs
    }
  where
    keep = filter (not . null . snd)

-- functions ------------------------------------------------------------

topFunBinds :: HsModule GhcPs -> [(Text, MatchGroup GhcPs (LHsExpr GhcPs))]
topFunBinds m =
  [ (rdrText (unLoc n), mg)
  | d <- hsmodDecls m
  , ValD _ (FunBind { fun_id = n, fun_matches = mg }) <- [unLoc d]
  ]

matchesOf :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Match GhcPs (LHsExpr GhcPs)]
matchesOf (MG { mg_alts = alts }) = map unLoc (unLoc alts)

rhssOf :: GRHSs GhcPs (LHsExpr GhcPs) -> [LGRHS GhcPs (LHsExpr GhcPs)]
rhssOf (GRHSs { grhssGRHSs = rs }) = rs

-- clauses --------------------------------------------------------------

-- | One line per clause that says something: a constructor or literal
-- pattern, or a guard. "pats | guard, guard"; "_" stands for an
-- all-variable pattern list on a guarded clause. The guard "otherwise"
-- is dropped.
clauseLines :: Match GhcPs (LHsExpr GhcPs) -> [Text]
clauseLines (Match { m_pats = pats, m_grhss = grhss }) =
  let patText     = T.unwords (map (oneLine . render . unLoc) pats)
      informative = any patInformative pats
      guardSets   =
        [ gs
        | GRHS _ guards _ <- map unLoc (rhssOf grhss)
        , let gs = filter (/= "otherwise") (map (oneLine . render . unLoc) guards)
        , not (null gs)
        ]
  in if null guardSets
       then [ clip patText | informative ]
       else [ clip ((if T.null patText then "_" else patText) <> " | " <> T.intercalate ", " gs)
            | gs <- guardSets ]

patInformative :: LPat GhcPs -> Bool
patInformative = SYB.everything (||) (SYB.mkQ False one)
  where
    one :: Pat GhcPs -> Bool
    one p = SYB.showConstr (SYB.toConstr p)
              `elem` ["ConPat", "LitPat", "NPat", "NPlusKPat"]

-- failures -------------------------------------------------------------

-- | Error constructors and error strings raised anywhere in the body:
-- the argument of a thrower, whether applied directly or through ($).
failsIn :: Data a => a -> [Text]
failsIn = nub . SYB.everything (++) (SYB.mkQ [] one)
  where
    one :: HsExpr GhcPs -> [Text]
    one = \case
      HsApp _ f x
        | isThrower (unLoc f) -> thrown (unLoc x)
      OpApp _ f op x
        | isDollar (unLoc op), isThrower (unLoc f) -> thrown (unLoc x)
      _ -> []

throwers :: [Text]
throwers =
  [ "throwError", "throwIO", "throwM", "throw", "throwE"
  , "Left", "fail", "error", "exitWith" ]

isThrower :: HsExpr GhcPs -> Bool
isThrower e = case appHead e of
  Just n  -> rdrText n `elem` throwers
  Nothing -> False

isDollar :: HsExpr GhcPs -> Bool
isDollar e = case appHead e of
  Just n  -> rdrText n == "$"
  Nothing -> False

thrown :: HsExpr GhcPs -> [Text]
thrown x = case appHead x of
  Just n | isDataOcc (rdrNameOcc n) -> [rdrText n]
  _ -> case stringLit x of
         Just s  -> ["\"" <> clip s <> "\""]
         Nothing -> []

-- | The head of an application chain, looking through parentheses, type
-- annotations and anything else with exactly one expression child.
appHead :: HsExpr GhcPs -> Maybe RdrName
appHead = \case
  HsVar _ n                  -> Just (unLoc n)
  HsApp _ f _                -> appHead (unLoc f)
  RecordCon { rcon_con = c } -> Just (unLoc c)
  e -> case childExprs e of
         [c] -> appHead (unLoc c)
         _   -> Nothing

childExprs :: HsExpr GhcPs -> [LHsExpr GhcPs]
childExprs = concat . SYB.gmapQ (SYB.mkQ [] (\(c :: LHsExpr GhcPs) -> [c]))

stringLit :: HsExpr GhcPs -> Maybe Text
stringLit = \case
  HsLit _ (HsString _ fs)                            -> Just (T.pack (unpackFS fs))
  HsOverLit _ (OverLit { ol_val = HsIsString _ fs }) -> Just (T.pack (unpackFS fs))
  e -> case childExprs e of
         [c] -> stringLit (unLoc c)
         _   -> Nothing

-- literals -------------------------------------------------------------

-- | Literals in a body worth a line: every number and character, and the
-- short strings that look like a token rather than a message (no space,
-- at most 22 characters including the quotes). 0, 1 and "" are dropped.
literalsIn :: Data a => a -> [Text]
literalsIn x =
  filter worthKeeping . nub $
    SYB.everything (++) (SYB.mkQ [] lit `SYB.extQ` olit) x
  where
    lit :: HsLit GhcPs -> [Text]
    lit l = [oneLine (render l)]
    olit :: HsOverLit GhcPs -> [Text]
    olit l = [oneLine (render l)]
    worthKeeping t
      | t `elem` ["0", "1", "\"\""] = False
      | "\"" `T.isPrefixOf` t       = T.length t <= 22 && not (T.any (== ' ') t)
      | otherwise                   = True

capped :: Int -> [Text] -> [Text]
capped n xs
  | length xs <= n = xs
  | otherwise      = take n xs <> ["+" <> T.pack (show (length xs - n)) <> " more"]

-- | A zero-argument binding with one unguarded clause whose body is a
-- literal, possibly negated.
constCandidate :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe Text
constCandidate mg = case matchesOf mg of
  [Match { m_pats = [], m_grhss = grhss }] ->
    case map unLoc (rhssOf grhss) of
      [GRHS _ [] body] -> literalBody (unLoc body)
      _                -> Nothing
  _ -> Nothing

literalBody :: HsExpr GhcPs -> Maybe Text
literalBody = \case
  HsLit _ l      -> Just (oneLine (render l))
  HsOverLit _ l  -> Just (oneLine (render l))
  NegApp _ e _   -> ("-" <>) <$> literalBody (unLoc e)
  e -> case childExprs e of
         [c] -> literalBody (unLoc c)
         _   -> Nothing

-- transitions ----------------------------------------------------------

-- | Edges from constructors matched to constructors built, for each
-- top-level clause and for each case alternative inside it. A clause or
-- alternative that matches no constructor of a type but builds one gives
-- an edge from "*".
edgesIn :: ConMap -> Text -> MatchGroup GhcPs (LHsExpr GhcPs) -> [DerivedEdge]
edgesIn conMap witness mg =
  nub $ concat
    [ edgesFor conMap witness (conPatsIn pats) (builtConsIn grhss)
        <> concat
             [ edgesFor conMap witness (conPatsIn apats) (builtConsIn agrhss)
             | HsCase _ _ amg <- SYB.listify isCase grhss
             , Match { m_pats = apats, m_grhss = agrhss } <- matchesOf amg
             ]
    | Match { m_pats = pats, m_grhss = grhss } <- matchesOf mg
    ]
  where
    isCase :: HsExpr GhcPs -> Bool
    isCase = \case
      HsCase {} -> True
      _         -> False

edgesFor :: ConMap -> Text -> [Text] -> [Text] -> [DerivedEdge]
edgesFor conMap witness froms tos =
  nub
    [ DerivedEdge ty f c witness
    | c <- tos
    , (m, t) <- Set.toList (typesOf c)
    , let ty = m <> "." <> t
    , let fs = [ f' | f' <- froms, Set.member (m, t) (typesOf f') ]
    , f <- if null fs then ["*"] else fs
    ]
  where
    typesOf c = Map.findWithDefault Set.empty c conMap

conPatsIn :: Data a => a -> [Text]
conPatsIn = nub . SYB.everything (++) (SYB.mkQ [] one)
  where
    one :: Pat GhcPs -> [Text]
    one = \case
      ConPat { pat_con = c } -> [rdrText (unLoc c)]
      _                      -> []

builtConsIn :: Data a => a -> [Text]
builtConsIn = nub . SYB.everything (++) (SYB.mkQ [] one)
  where
    one :: HsExpr GhcPs -> [Text]
    one = \case
      HsVar _ n
        | isDataOcc (rdrNameOcc (unLoc n)) -> [rdrText (unLoc n)]
      RecordCon { rcon_con = c }           -> [rdrText (unLoc c)]
      _                                    -> []

-- abstract types -------------------------------------------------------

-- | Data types the export list exports without all their constructors,
-- with the exported functions whose result type names them. A module
-- with no export list exports everything and has none.
abstractTypesOf :: HsModule GhcPs -> [Signature] -> [AbstractType]
abstractTypesOf m sigs = case hsmodExports m of
  Nothing -> []
  Just lexports ->
    let ies = map unLoc (unLoc lexports)
        kindOf ie = SYB.showConstr (SYB.toConstr ie)
        headName ie = case ieNames ie of
          (n : _) -> Just (rdrText n)
          []      -> Nothing
        absNames  = [ n | ie <- ies, kindOf ie == "IEThingAbs", Just n <- [headName ie] ]
        withNames = [ (n, map rdrText subs, ".." `T.isInfixOf` render ie)
                    | ie <- ies, kindOf ie == "IEThingWith"
                    , (hd : subs) <- [ieNames ie], let n = rdrText hd ]
        varNames  = [ n | ie <- ies, kindOf ie == "IEVar", Just n <- [headName ie] ]
        hidden t cons
          | t `elem` absNames = cons
          | otherwise = case [ (subs, wild) | (n, subs, wild) <- withNames, n == t ] of
              ((subs, wild) : _)
                | wild      -> []
                | otherwise -> [ c | c <- cons, c `notElem` subs ]
              [] -> []
    in [ AbstractType t h makers
       | (t, cons) <- dataTypesOf m
       , not (null cons)
       , let h = hidden t cons
       , not (null h)
       , let makers = [ sigName s
                      | s <- sigs
                      , sigName s `elem` varNames
                      , resultMentions t (sigVerbatim s) ]
       ]

-- | Whether the last arrow-separated segment of a signature contains the
-- type name as a token.
resultMentions :: Text -> Text -> Bool
resultMentions t sig =
  let afterColons = case T.breakOn "::" sig of
        (_, rest) | not (T.null rest) -> T.drop 2 rest
        _                             -> sig
      segments = T.splitOn "->" afterColons
      lastSeg  = if null segments then "" else last segments
      tokens   = T.split (\c -> not (isAlphaNum c || c == '_' || c == '\'')) lastSeg
  in t `elem` tokens

-- helpers --------------------------------------------------------------

render :: Outputable a => a -> Text
render = T.pack . showSDocUnsafe . ppr

oneLine :: Text -> Text
oneLine = T.unwords . T.words

clip :: Text -> Text
clip t
  | T.length t <= 120 = t
  | otherwise         = T.take 117 t <> "..."

rdrText :: RdrName -> Text
rdrText = T.pack . occNameString . rdrNameOcc