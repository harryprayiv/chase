{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Client for the local Unison codebase HTTP API (the server ucm runs
-- for the Local UI). Decoders are deliberately minimal and tolerant of
-- unknown fields: we decode only what chase needs, so an API revision
-- that adds fields does not break us.
module Chase.Unison.Api
  ( -- * Connection
    Codebase (..)
  , newCodebase
    -- * Endpoints
  , listNamespace
  , getDefinition
  , getDependents
    -- * Listing
  , Listing (..)
  , Child (..)
  , ChildKind (..)
    -- * Definitions
  , DefinitionResponse (..)
  , TermDef (..)
  , TypeDef (..)
  , DisplayObject (..)
  , displayObjectSegments
    -- * Dependents
  , Dependent (..)
    -- * Syntax segments
  , Segment (..)
  , Annotation (..)
  , renderSegments
  , segmentRefs
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

-- ---------------------------------------------------------------------------
-- Connection
-- ---------------------------------------------------------------------------

-- | A handle to one project/branch of a running codebase server.
-- 'cbBaseUrl' is everything up to and including the token, no trailing
-- slash, e.g. @http://127.0.0.1:5858/codebase@.
data Codebase = Codebase
  { cbManager :: Manager
  , cbBaseUrl :: String
  , cbProject :: Text
  , cbBranch  :: Text
  }

newCodebase :: String -> Text -> Text -> IO Codebase
newCodebase baseUrl project branch = do
  mgr <- newManager defaultManagerSettings
  pure (Codebase mgr (dropTrailingSlash baseUrl) project branch)
  where
    dropTrailingSlash s = case reverse s of
      ('/' : rest) -> reverse rest
      _            -> s

-- | GET an endpoint under the project/branch path, decode the JSON body.
apiGet :: FromJSON a => Codebase -> String -> [(ByteString, Maybe Text)] -> IO (Either Text a)
apiGet cb endpoint params = do
  let url = cbBaseUrl cb
          <> "/api/projects/" <> T.unpack (cbProject cb)
          <> "/branches/"     <> T.unpack (cbBranch cb)
          <> "/"              <> endpoint
  reqE <- try (parseRequest url) :: IO (Either SomeException Request)
  case reqE of
    Left e     -> pure (Left ("bad request url: " <> T.pack (show e)))
    Right req0 -> do
      let req = setQueryString (mkParams params) req0
      respE <- try (httpLbs req (cbManager cb)) :: IO (Either SomeException (Response LBS.ByteString))
      case respE of
        Left e     -> pure (Left ("http error: " <> T.pack (show e)))
        Right resp ->
          let code = statusCode (responseStatus resp)
              body = responseBody resp
          in if code >= 200 && code < 300
               then case eitherDecode body of
                      Left err -> pure (Left ("decode error: " <> T.pack err))
                      Right a  -> pure (Right a)
               else pure (Left ("http " <> T.pack (show code) <> ": " <> showBody body))

mkParams :: [(ByteString, Maybe Text)] -> [(ByteString, Maybe ByteString)]
mkParams = mapMaybe (\(k, mv) -> (\v -> (k, Just (TE.encodeUtf8 v))) <$> mv)

showBody :: LBS.ByteString -> Text
showBody = TE.decodeUtf8With TEE.lenientDecode . LBS.toStrict

-- ---------------------------------------------------------------------------
-- Endpoints
-- ---------------------------------------------------------------------------

-- | @list?namespace=&relativeTo=@. With no namespace, lists the branch root.
listNamespace :: Codebase -> Maybe Text -> Maybe Text -> IO (Either Text Listing)
listNamespace cb ns relTo =
  apiGet cb "list" [("namespace", ns), ("relativeTo", relTo)]

-- | @getDefinition?names=&relativeTo=@. A name beginning with @#@ is a hash;
-- query-string encoding is handled for you.
getDefinition :: Codebase -> Text -> Maybe Text -> IO (Either Text DefinitionResponse)
getDefinition cb name relTo =
  apiGet cb "getDefinition" [("names", Just name), ("relativeTo", relTo)]

-- | @getDefinitionDependents?name=@. The exact reverse-dependency edges.
getDependents :: Codebase -> Text -> IO (Either Text [Dependent])
getDependents cb name = do
  r <- apiGet cb "getDefinitionDependents" [("name", Just name)]
  pure (fmap depResults r)

-- ---------------------------------------------------------------------------
-- Syntax segments
-- ---------------------------------------------------------------------------

-- | One annotated chunk of rendered source. Concatenating 'segText' over a
-- list reproduces the source exactly.
data Segment = Segment
  { segText       :: Text
  , segAnnotation :: Maybe Annotation
  } deriving (Show)

data Annotation = Annotation
  { annTag      :: Text        -- ^ e.g. TypeReference, TermReference, HashQualifier
  , annContents :: Maybe Text  -- ^ on a reference, the target hash
  , annFqn      :: Maybe Text  -- ^ on a reference, the fully-qualified name
  } deriving (Show)

instance FromJSON Segment where
  parseJSON = withObject "Segment" $ \o ->
    Segment <$> o .: "segment" <*> o .:? "annotation"

instance FromJSON Annotation where
  parseJSON = withObject "Annotation" $ \o -> do
    tag <- o .: "tag"
    mc  <- o .:? "contents"
    fqn <- o .:? "fqn"
    pure (Annotation tag (mc >>= valueToText) fqn)

valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText _          = Nothing

renderSegments :: [Segment] -> Text
renderSegments = T.concat . map segText

-- | (fqn, hash) for every reference-tagged segment. This is the dependency
-- set embedded in the rendered source itself, independent of getDependents.
segmentRefs :: [Segment] -> [(Text, Text)]
segmentRefs = mapMaybe $ \s -> do
  a <- segAnnotation s
  if "Reference" `T.isSuffixOf` annTag a
    then (,) <$> annFqn a <*> annContents a
    else Nothing

-- ---------------------------------------------------------------------------
-- Listing
-- ---------------------------------------------------------------------------

data Listing = Listing
  { listFqn      :: Text
  , listHash     :: Text
  , listChildren :: [Child]
  } deriving (Show)

-- | Every child carries a name and hash; the variant-specific payload lives
-- in 'childKind'. Shared fields are total selectors; the kind sub-sum is
-- positional, so there are no partial record selectors.
data Child = Child
  { childName :: Text
  , childHash :: Text
  , childKind :: ChildKind
  } deriving (Show)

data ChildKind
  = KTerm Text [Segment]  -- ^ term tag, type signature
  | KType Text            -- ^ type tag
  | KNamespace Int        -- ^ subnamespace size
  deriving (Show)

instance FromJSON Listing where
  parseJSON = withObject "Listing" $ \o ->
    Listing
      <$> o .: "namespaceListingFQN"
      <*> o .: "namespaceListingHash"
      <*> o .: "namespaceListingChildren"

instance FromJSON Child where
  parseJSON = withObject "Child" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    c   <- o .: "contents"
    case tag of
      "TermObject" -> flip (withObject "TermObject") c $ \t -> do
        n  <- t .: "termName"
        h  <- t .: "termHash"
        tg <- t .: "termTag"
        ty <- fromMaybe [] <$> t .:? "termType"
        pure (Child n h (KTerm tg ty))
      "TypeObject" -> flip (withObject "TypeObject") c $ \t -> do
        n  <- t .: "typeName"
        h  <- t .: "typeHash"
        tg <- t .: "typeTag"
        pure (Child n h (KType tg))
      "Subnamespace" -> flip (withObject "Subnamespace") c $ \t -> do
        n  <- t .: "namespaceName"
        h  <- t .: "namespaceHash"
        sz <- t .: "namespaceSize"
        pure (Child n h (KNamespace sz))
      other -> fail ("unknown child tag: " <> T.unpack other)

-- ---------------------------------------------------------------------------
-- Definitions
-- ---------------------------------------------------------------------------

data DefinitionResponse = DefinitionResponse
  { drMissing :: [Text]
  , drTerms   :: Map Text TermDef  -- ^ keyed by hash
  , drTypes   :: Map Text TypeDef  -- ^ keyed by hash
  } deriving (Show)

-- | One hash, many names. 'tdNames' is the full alias set.
data TermDef = TermDef
  { tdNames      :: [Text]
  , tdBestName   :: Text
  , tdTag        :: Text
  , tdSignature  :: [Segment]
  , tdDefinition :: DisplayObject
  } deriving (Show)

data TypeDef = TypeDef
  { tydNames      :: [Text]
  , tydBestName   :: Text
  , tydTag        :: Text
  , tydDefinition :: DisplayObject
  } deriving (Show)

-- | A rendered definition body, builtin marker, or a hash for a missing one.
data DisplayObject
  = UserObject    [Segment]
  | BuiltinObject [Segment]
  | MissingObject Text
  deriving (Show)

displayObjectSegments :: DisplayObject -> [Segment]
displayObjectSegments = \case
  UserObject s    -> s
  BuiltinObject s -> s
  MissingObject _ -> []

instance FromJSON DefinitionResponse where
  parseJSON = withObject "DefinitionResponse" $ \o ->
    DefinitionResponse
      <$> (fromMaybe [] <$> o .:? "missingDefinitions")
      <*> o .: "termDefinitions"
      <*> o .: "typeDefinitions"

instance FromJSON TermDef where
  parseJSON = withObject "TermDef" $ \o ->
    TermDef
      <$> o .: "termNames"
      <*> o .: "bestTermName"
      <*> o .: "defnTermTag"
      <*> (fromMaybe [] <$> o .:? "signature")
      <*> o .: "termDefinition"

instance FromJSON TypeDef where
  parseJSON = withObject "TypeDef" $ \o ->
    TypeDef
      <$> o .: "typeNames"
      <*> o .: "bestTypeName"
      <*> o .: "defnTypeTag"
      <*> o .: "typeDefinition"

instance FromJSON DisplayObject where
  parseJSON = withObject "DisplayObject" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "UserObject"    -> UserObject    <$> o .: "contents"
      "BuiltinObject" -> BuiltinObject <$> o .: "contents"
      "MissingObject" -> MissingObject <$> o .: "contents"
      other           -> fail ("unknown DisplayObject tag: " <> T.unpack other)

-- ---------------------------------------------------------------------------
-- Dependents
-- ---------------------------------------------------------------------------

data Dependent = Dependent
  { depFqn         :: Text
  , depKind        :: Text  -- ^ "term" | "type"
  , depHash        :: Text
  , depDisplayName :: Text
  } deriving (Show)

newtype DependentsResponse = DependentsResponse { depResults :: [Dependent] }

instance FromJSON DependentsResponse where
  parseJSON = withObject "DependentsResponse" $ \o ->
    DependentsResponse <$> o .: "results"

instance FromJSON Dependent where
  parseJSON = withObject "Dependent" $ \o -> do
    fqn  <- o .: "fqn"
    kind <- o .: "kind"
    def  <- o .: "definition"
    flip (withObject "definition") def $ \d ->
      Dependent fqn kind <$> d .: "hash" <*> d .: "displayName"