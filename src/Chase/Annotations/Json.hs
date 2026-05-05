{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Load 'ModuleAnnotations' from a single JSON file. The shape:
--
-- > {
-- >   "version": 1,
-- >   "modules": {
-- >     "DB.Auth": {
-- >       "decisions": [
-- >         { "name": "...", "what": "...",
-- >           "why": "..." | ["...","..."],
-- >           "affects": ["fn1", "fn2"] }
-- >       ],
-- >       "openIssues": [
-- >         { "name": "...", "what": "...",
-- >           "why": "..." | ["...","..."],
-- >           "blocking": ["downstream feature", "..."],
-- >           "affects": ["fn1", "fn2"] }
-- >       ],
-- >       "invariants": {
-- >         "hashPassword": ["line one", "line two"],
-- >         "createSession": {
-- >           "intent": "...",
-- >           "effects": ["DBPool", "IO"],
-- >           "notes":  ["..."],
-- >           "spec":   [{"input": "...", "expected": "..."}],
-- >           "consumes": ["Server.Auth.loginHandler", "..."],
-- >           "hint":   "body: ~30 lines, see source"
-- >         }
-- >       },
-- >       "constants": [
-- >         { "name": "...", "value": "...",
-- >           "notes": ["plain note", "BUG: bug note"] }
-- >       ],
-- >       "topologies": [
-- >         { "name": "...", "vertices": ["A","B"],
-- >           "transitions": { "A": ["B"], "B": ["A"] } }
-- >       ]
-- >     }
-- >   }
-- > }
module Chase.Annotations.Json
  ( loadAnnotations
  , loadAnnotationsIfExists
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (doesFileExist)

import Chase.Types

-- | Load annotations from a JSON file. Returns Left if the file is
-- missing or fails to parse, with a message suitable for stderr.
loadAnnotations
  :: FilePath -> IO (Either String (Map Text ModuleAnnotations))
loadAnnotations path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "annotations file not found: " <> path
    else do
      bs <- LBS.readFile path
      pure $ case A.eitherDecode bs of
        Left  e -> Left  $ path <> ": " <> e
        Right v -> case AT.parse parseRoot v of
          AT.Error   e -> Left  $ path <> ": " <> e
          AT.Success m -> Right m

-- | If the file exists and parses, return its contents.
-- If missing, return empty silently. Parse errors are also silenced
-- here -- use 'loadAnnotations' if you want to surface them.
loadAnnotationsIfExists :: FilePath -> IO (Map Text ModuleAnnotations)
loadAnnotationsIfExists path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else either (const Map.empty) id <$> loadAnnotations path

-- top level ------------------------------------------------------------

parseRoot :: A.Value -> AT.Parser (Map Text ModuleAnnotations)
parseRoot = A.withObject "AnnotationsFile" $ \o -> do
  ver <- o A..:? "version" A..!= (1 :: Int)
  if ver /= 1
    then fail $ "unsupported annotations version: " <> show ver
    else (o A..: "modules") >>= parseModulesMap

parseModulesMap :: A.Object -> AT.Parser (Map Text ModuleAnnotations)
parseModulesMap obj =
  Map.fromList <$> mapM parsePair (KM.toList obj)
  where
    parsePair (k, v) = do
      let modName = Key.toText k
      ann <- parseModule modName v
      pure (modName, ann)

parseModule :: Text -> A.Value -> AT.Parser ModuleAnnotations
parseModule modName =
  A.withObject ("module " <> T.unpack modName) $ \o -> do
    consts  <- o A..:? "constants"  A..!= []
    invsObj <- o A..:? "invariants" A..!= KM.empty
    decs    <- o A..:? "decisions"  A..!= []
    issues  <- o A..:? "openIssues" A..!= []
    tops    <- o A..:? "topologies" A..!= []
    pConsts <- mapM parseConstant consts
    pInvs   <- parseInvariantsMap invsObj
    pDecs   <- mapM parseDecision decs
    pIssues <- mapM parseOpenIssue issues
    pTops   <- mapM parseTopology tops
    pure ModuleAnnotations
      { annModName    = modName
      , annConstants  = pConsts
      , annInvariants = pInvs
      , annDecisions  = pDecs
      , annOpenIssues = pIssues
      , annTopologies = pTops
      }

-- constants ------------------------------------------------------------

parseConstant :: A.Value -> AT.Parser Constant
parseConstant = A.withObject "constant" $ \o -> do
  name  <- o A..:  "name"
  value <- o A..:  "value"
  notes <- o A..:? "notes" A..!= []
  pure Constant
    { constName     = name
    , constValueDoc = value
    , constNotes    = map noteFromText notes
    }

noteFromText :: Text -> Note
noteFromText t = case T.stripPrefix "BUG: " t of
  Just rest -> BugNote rest
  Nothing   -> Note t

-- invariants -----------------------------------------------------------

-- | Each invariant value is either:
--
--   * an array of strings (the @!@ lines verbatim, no consumes), or
--   * an object with optional fields @intent@, @effects@, @notes@,
--     @spec@, @consumes@, @hint@.
parseInvariantsMap :: A.Object -> AT.Parser [Invariant]
parseInvariantsMap obj = mapM parseOne (KM.toList obj)
  where
    parseOne (k, v) = parseInvariantValue (Key.toText k) v

parseInvariantValue :: Text -> A.Value -> AT.Parser Invariant
parseInvariantValue fnName = \case
  v@(A.Array _) -> inv fnName <$> AT.parseJSON v
  A.Object o    -> parseObjectInvariant fnName o
  _             -> fail $ "invariants." <> T.unpack fnName
                   <> " must be an array of strings or an object"

parseObjectInvariant :: Text -> A.Object -> AT.Parser Invariant
parseObjectInvariant fnName o = do
  mIntent  <- o A..:? "intent"
  effects  <- o A..:? "effects"  A..!= ([] :: [Text])
  notes    <- o A..:? "notes"    A..!= ([] :: [Text])
  specs    <- o A..:? "spec"     A..!= ([] :: [SpecEntry])
  consumes <- o A..:? "consumes" A..!= ([] :: [Text])
  hint     <- o A..:? "hint"
  let intentLine = maybe [] (\t -> ["intent: " <> t]) mIntent
      effectLine =
        if null effects
          then []
          else ["effects: " <> T.intercalate ", " effects]
      lns = intentLine <> effectLine <> notes <> map renderSpec specs
  pure $ Invariant fnName lns consumes hint

data SpecEntry = SpecEntry
  { seInput    :: Text
  , seExpected :: Text
  }

instance A.FromJSON SpecEntry where
  parseJSON = A.withObject "spec" $ \o -> SpecEntry
    <$> o A..: "input"
    <*> o A..: "expected"

renderSpec :: SpecEntry -> Text
renderSpec SpecEntry{..} = "spec: " <> seInput <> " => " <> seExpected

-- decisions ------------------------------------------------------------

parseDecision :: A.Value -> AT.Parser Decision
parseDecision = A.withObject "decision" $ \o -> do
  name    <- o A..:  "name"
  what    <- o A..:  "what"
  whyVal  <- o A..:? "why" A..!= A.String ""
  affects <- o A..:? "affects" A..!= []
  why <- parseWhy "decision.why" whyVal
  pure $ Decision name what why affects

-- open issues ----------------------------------------------------------

-- | Same shape as a decision but with an additional 'blocking' list.
-- 'why' accepts the same string-or-array form that decisions do.
parseOpenIssue :: A.Value -> AT.Parser OpenIssue
parseOpenIssue = A.withObject "openIssue" $ \o -> do
  name     <- o A..:  "name"
  what     <- o A..:  "what"
  whyVal   <- o A..:? "why"      A..!= A.String ""
  blocking <- o A..:? "blocking" A..!= []
  affects  <- o A..:? "affects"  A..!= []
  why <- parseWhy "openIssue.why" whyVal
  pure $ OpenIssue name what why blocking affects

-- | Shared by 'parseDecision' and 'parseOpenIssue': accept either a
-- bare string or an array of strings, joining the array with single
-- spaces. Authors should embed their own punctuation in array entries
-- if they want sentence breaks.
parseWhy :: String -> A.Value -> AT.Parser Text
parseWhy ctx = \case
  A.String s    -> pure s
  a@(A.Array _) -> T.intercalate " " <$> AT.parseJSON a
  _ -> fail $ ctx <> " must be a string or array of strings"

-- topologies -----------------------------------------------------------

parseTopology :: A.Value -> AT.Parser Topology
parseTopology = A.withObject "topology" $ \o -> do
  name     <- o A..: "name"
  vertices <- o A..: "vertices"
  transObj <- o A..: "transitions"
  trans    <- mapM parseTransPair (KM.toList transObj)
  pure Topology
    { topName        = name
    , topVertices    = vertices
    , topTransitions = trans
    }
  where
    parseTransPair (k, v) = do
      tos <- AT.parseJSON v
      pure (Key.toText k, tos)