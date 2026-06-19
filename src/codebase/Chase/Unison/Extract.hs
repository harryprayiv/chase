-- src-codebase/Chase/Unison/Extract.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chase.Unison.Extract
  ( UnisonSkeleton (..)
  , SkelEntry (..)
  , SkelKind (..)
  , collectSkeleton
  , renderSkeleton
  , defaultSkip
  , isDependencyNamespace
  , shouldSkipNamespace
  , resolveConcurrency
  ) where

import Chase.Unison.Api
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

-- | How many single-definition fetches run at once. This is the only lever on
-- first-run wall time, and it is bounded by UCM's memory, not CPU: each in-flight
-- request holds a full pretty-print-environment build. 16 OOM-killed the server.
-- 4 is the safe default; climb toward 8 while watching UCM's memory.
resolveConcurrency :: Int
resolveConcurrency = 4

-- | How many times to retry a single failed fetch before degrading it.
resolveRetries :: Int
resolveRetries = 3

-- | Exact local namespace names to skip outright.
defaultSkip :: [Text]
defaultSkip = ["lib"]

-- | A namespace name shaped like a vendored dependency, e.g.
-- @runarorama_postgres_2_5_1@. Heuristic: the trailing three
-- underscore-separated components are all numeric (a @_MAJOR_MINOR_PATCH@
-- version suffix). Catches top-level deps that never lived under @lib@.
isDependencyNamespace :: Text -> Bool
isDependencyNamespace n =
  case reverse (T.splitOn "_" n) of
    (a : b : c : _) -> all isNumeric [a, b, c]
    _               -> False
  where
    isNumeric t = not (T.null t) && T.all isDigit t

-- | True if a namespace's local name should be pruned from the walk.
shouldSkipNamespace :: [Text] -> Text -> Bool
shouldSkipNamespace skip name =
  name `elem` skip || isDependencyNamespace name


data UnisonSkeleton = UnisonSkeleton
  { uskProject :: Text
  , uskBranch  :: Text
  , uskRoot    :: Text          -- ^ "" means the whole branch
  , uskEntries :: [SkelEntry]   -- ^ deduplicated by hash, sorted by best name
  } deriving (Show)

data SkelEntry = SkelEntry
  { seHash  :: Text     -- ^ content hash from the listing (used for display + dedup)
  , seNames :: [Text]   -- ^ all in-scope full names, best (least-qualified) first
  , seKind  :: SkelKind
  } deriving (Show)

data SkelKind
  = SkelTerm Text             -- ^ rendered, name-resolved type signature
  | SkelType Text Text [Text] -- ^ type tag, rendered declaration, folded record field names
  deriving (Show)


-- | Walk-time accumulator. The walk records what to resolve (kind, listing
-- tag, listing fallback) but does NOT fetch.
data Entry = Entry
  { eNames :: [Text]        -- ^ accumulated, unsorted
  , eReq   :: ResolveReq
  }

data ResolveReq
  = ReqTerm Text [Segment]   -- ^ listing term tag, listing-side fallback signature
  | ReqType Text             -- ^ type tag

type Acc = Map Text Entry

collectSkeleton :: Codebase -> Text -> IO (Either Text UnisonSkeleton)
collectSkeleton cb root = do
  accE <- walkNs cb defaultSkip root Map.empty
  case accE of
    Left err  -> pure (Left err)
    Right acc -> do
      let (toFetch, fields) = planResolution acc
          total    = Map.size acc
          fetching = length toFetch
      hPutStrLn stderr
        ( "chase-unison: " ++ show total ++ " definitions, "
            ++ show fetching ++ " to resolve ("
            ++ show (total - fetching) ++ " accessors/constructors dropped), "
            ++ "concurrency " ++ show resolveConcurrency )
      results <- mapPool resolveConcurrency (resolveEntry cb) toFetch
      let entries  = map fst results
          failures = [ m | (_, Just m) <- results ]
      reportFailures failures
      pure (Right (buildSkeleton cb root fields entries))

reportFailures :: [Text] -> IO ()
reportFailures [] = pure ()
reportFailures fs = do
  hPutStrLn stderr
    ( "chase-unison: WARNING " ++ show (length fs)
        ++ " definition(s) failed to resolve, rendered hash-only." )
  hPutStrLn stderr "chase-unison: if this is high, set resolveConcurrency = 1 and re-run."
  mapM_ (\m -> hPutStrLn stderr ("  - " ++ T.unpack m)) (take 10 fs)
  if length fs > 10
    then hPutStrLn stderr ("  ... and " ++ show (length fs - 10) ++ " more")
    else pure ()


-- | The walk is fetch-free: it only lists namespaces (one call each) and
-- accumulates hash -> names, recording each definition's kind and tag.
walkNs :: Codebase -> [Text] -> Text -> Acc -> IO (Either Text Acc)
walkNs cb skip ns acc0 = do
  let q = if T.null ns then Nothing else Just ns
  listNamespace cb q Nothing >>= \case
    Left err -> pure (Left ("list " <> nsLabel ns <> ": " <> err))
    Right l  -> foldChildren cb skip ns (listChildren l) acc0

foldChildren :: Codebase -> [Text] -> Text -> [Child] -> Acc -> IO (Either Text Acc)
foldChildren _ _ _ [] acc = pure (Right acc)
foldChildren cb skip ns (c : cs) acc =
  handleChild cb skip ns c acc >>= \case
    Left err   -> pure (Left err)
    Right acc' -> foldChildren cb skip ns cs acc'

handleChild :: Codebase -> [Text] -> Text -> Child -> Acc -> IO (Either Text Acc)
handleChild cb skip ns c acc =
  let h        = childHash c
      fullName = joinNs ns (childName c)
  in case childKind c of
       KTerm tag listingSig ->
         pure (Right (addReq h (ReqTerm tag listingSig) fullName acc))
       KType tag ->
         pure (Right (addReq h (ReqType tag) fullName acc))
       KNamespace _ ->
         if shouldSkipNamespace skip (childName c)
           then pure (Right acc)
           else walkNs cb skip fullName acc

-- | First-seen kind for a hash wins; later sightings only add names (the aka set).
addReq :: Text -> ResolveReq -> Text -> Acc -> Acc
addReq h req name = Map.insertWith mergeE h (Entry [name] req)
  where mergeE new old = old { eNames = eNames new ++ eNames old }


-- | Decide, before any network traffic, which definitions to fetch and which
-- to drop as redundant. Dropped: data/ability constructors (the type
-- declaration already shows them with names resolved), record-accessor
-- set/modify satellites, and accessor getters whose owning type is in scope
-- (the field name is folded onto the type instead). Returns (entries to
-- fetch, typeHash -> folded field names).
planResolution :: Acc -> ([(Text, Entry)], Map Text [Text])
planResolution acc =
  (toFetch, fields)
  where
    entries = Map.toList acc

    termNames =
      Set.fromList [ n | (_, e) <- entries, isTermReq (eReq e), n <- eNames e ]
    typeNameToHash =
      Map.fromList [ (n, h) | (h, e) <- entries, isTypeReq (eReq e), n <- eNames e ]

    isGetterName nm =
      T.isInfixOf "." nm
        && Set.member (nm <> ".set") termNames
        && Set.member (nm <> ".modify") termNames
    isSatelliteName nm = maybe False isGetterName (stripAccessorSuffix nm)

    ownerHashOfAny names =
      listToMaybe
        [ tyH | n <- names, Just tyH <- [Map.lookup (dropLastSeg n) typeNameToHash] ]

    classified = map classify entries
    classify (h, e)
      | isTypeReq (eReq e)        = KeepFetch (h, e)   -- types always fetched
      | isConstructorReq (eReq e) = DropIt             -- redundant with the decl
      | isSatelliteName nm        = DropIt
      | isGetterName nm =
          case ownerHashOfAny (eNames e) of
            Just tyH -> Fold tyH (lastSeg nm)
            Nothing  -> KeepFetch (h, e)
      | otherwise = KeepFetch (h, e)
      where nm = bestName (sortNames (eNames e))

    toFetch = [ p | KeepFetch p <- classified ]
    fields  = Map.fromListWith (++) [ (tyH, [f]) | Fold tyH f <- classified ]

data PlanItem = KeepFetch (Text, Entry) | DropIt | Fold Text Text

isTermReq :: ResolveReq -> Bool
isTermReq (ReqTerm _ _) = True
isTermReq _             = False

isTypeReq :: ResolveReq -> Bool
isTypeReq (ReqType _) = True
isTypeReq _           = False

-- | A constructor's listing tag contains "constructor" (data or ability
-- constructor). Matched case-insensitively; if Unison uses a different label,
-- these terms simply stay in the output rendered hash-only, as before.
isConstructorReq :: ResolveReq -> Bool
isConstructorReq (ReqTerm tag _) = T.isInfixOf "constructor" (T.toLower tag)
isConstructorReq _               = False


-- | Resolve one definition by its most-qualified (unique) name. Failure is
-- retried with backoff, then degraded to a hash-only fallback and reported,
-- never silently passed off as success.
resolveEntry :: Codebase -> (Text, Entry) -> IO (SkelEntry, Maybe Text)
resolveEntry cb (h, e) =
  let names   = sortNames (eNames e)
      reqName = mostQualified names
  in case eReq e of
       ReqTerm _ fallback -> do
         r <- fetchWithRetry cb reqName resolveRetries
         case r of
           Right dr | Just td <- firstValue (drTerms dr) ->
             pure (SkelEntry h names (SkelTerm (oneLineSig (renderSegments (tdSignature td)))), Nothing)
           Right _ ->
             pure ( SkelEntry h names (SkelTerm (oneLineSig (renderSegments fallback)))
                  , Just (reqName <> ": empty response") )
           Left err ->
             pure ( SkelEntry h names (SkelTerm (oneLineSig (renderSegments fallback)))
                  , Just (reqName <> ": " <> err) )
       ReqType tag -> do
         r <- fetchWithRetry cb reqName resolveRetries
         case r of
           Right dr | Just td <- firstValue (drTypes dr) ->
             pure (SkelEntry h names (SkelType tag (renderSegments (displayObjectSegments (tydDefinition td))) []), Nothing)
           Right _ ->
             pure (SkelEntry h names (SkelType tag "" []), Just (reqName <> ": empty response"))
           Left err ->
             pure (SkelEntry h names (SkelType tag "" []), Just (reqName <> ": " <> err))

fetchWithRetry :: Codebase -> Text -> Int -> IO (Either Text DefinitionResponse)
fetchWithRetry cb name attempts = loop attempts 250000
  where
    loop n delayUs = do
      r <- getDefinition cb name Nothing
      case r of
        Right dr -> pure (Right dr)
        Left err
          | n <= 1    -> pure (Left err)
          | otherwise -> threadDelay delayUs >> loop (n - 1) (delayUs * 2)

-- | The response map for a single-name getDefinition has zero or one entries.
-- Taking the first value is key-format-agnostic (no short/full hash matching).
firstValue :: Map k v -> Maybe v
firstValue m = case Map.elems m of
  (v : _) -> Just v
  []      -> Nothing

mostQualified :: [Text] -> Text
mostQualified [] = ""
mostQualified xs = last xs

-- | Bounded concurrent map. Forks one green thread per input but only lets
-- @n@ run the action at once, via a counting semaphore.
mapPool :: Int -> (a -> IO b) -> [a] -> IO [b]
mapPool n f xs = do
  sem <- newQSem n
  mapConcurrently (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) xs

-- | Collapse signature whitespace (incl. newlines) to single spaces so a
-- multi-line type renders as one skeleton line.
oneLineSig :: Text -> Text
oneLineSig = T.unwords . T.words


buildSkeleton :: Codebase -> Text -> Map Text [Text] -> [SkelEntry] -> UnisonSkeleton
buildSkeleton cb root fields entries =
  UnisonSkeleton
    { uskProject = cbProject cb
    , uskBranch  = cbBranch cb
    , uskRoot    = root
    , uskEntries = sortOn (bestName . seNames) (map (attachFields fields) entries)
    }

-- | Fold accessor-derived field names onto their owning type entry.
attachFields :: Map Text [Text] -> SkelEntry -> SkelEntry
attachFields fields e = case seKind e of
  SkelType tag decl flds ->
    case Map.lookup (seHash e) fields of
      Just newFlds -> e { seKind = SkelType tag decl (dedupSort (flds ++ newFlds)) }
      Nothing      -> e
  _ -> e


stripAccessorSuffix :: Text -> Maybe Text
stripAccessorSuffix nm =
  case T.stripSuffix ".set" nm of
    Just b  -> Just b
    Nothing -> T.stripSuffix ".modify" nm

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

dropLastSeg :: Text -> Text
dropLastSeg n = T.intercalate "." (safeInit (T.splitOn "." n))
  where
    safeInit [] = []
    safeInit xs = init xs

dedupSort :: [Text] -> [Text]
dedupSort = Set.toList . Set.fromList


sortNames :: [Text] -> [Text]
sortNames = sortOn (\n -> (T.count "." n, T.length n, n))

bestName :: [Text] -> Text
bestName (n : _) = n
bestName []      = "(anonymous)"

renderSkeleton :: UnisonSkeleton -> Text
renderSkeleton usk =
  T.unlines $
    [ "%codebase " <> uskProject usk <> "/" <> uskBranch usk
    , "%root " <> (if T.null (uskRoot usk) then "(whole branch)" else uskRoot usk)
    , "%defs " <> T.pack (show (length (uskEntries usk)))
    , ""
    ]
      ++ concatMap renderEntry (uskEntries usk)

renderEntry :: SkelEntry -> [Text]
renderEntry se = body ++ akaLines ++ [""]
  where
    best = bestName (seNames se)
    sh   = "~" <> T.take 10 (T.dropWhile (== '#') (seHash se))
    body = case seKind se of
      SkelTerm sig ->
        [best <> " : " <> sig <> "   " <> sh]
      SkelType tag decl flds ->
        ("type[" <> tag <> "] " <> best <> "   " <> sh)
          : map ("  " <>) (declLines decl)
          ++ fieldLines flds
    declLines d   = if T.null d then [] else T.lines d
    fieldLines [] = []
    fieldLines fs = ["  fields: " <> T.intercalate ", " fs]
    aka      = filter (/= best) (seNames se)
    akaLines = ["  > aka: " <> T.intercalate ", " aka | not (null aka)]


joinNs :: Text -> Text -> Text
joinNs parent seg
  | T.null parent = seg
  | otherwise     = parent <> "." <> seg

nsLabel :: Text -> Text
nsLabel ns = if T.null ns then "(root)" else ns