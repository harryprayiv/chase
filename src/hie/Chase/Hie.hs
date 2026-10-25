{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | HIE files, read with the GHC that builds chase, reduced to a model
-- with no GHC types in it.
--
-- A .hie file is the compiler's own record of a module after renaming
-- and type checking: every identifier resolved to the module that
-- defines it, the context it occurs in, its type, and the node kind at
-- every span. GHC writes one per module when -fwrite-ide-info is on. The
-- format is tied to the GHC version that wrote it, and reading it needs
-- the same version, so this sublibrary depends on the ghc package itself
-- and the flake's compiler setting is where that version is chosen. A
-- file from another GHC fails to read and is reported with its path.
--
-- The rest of chase depends on this module only through the plain types
-- below, so ghc and ghc-lib-parser never meet in one component.
module Chase.Hie
  ( HieModule (..)
  , HieBinding (..)
  , HieRef (..)
  , RefKind (..)
  , findHieFiles
  , readHieDir
  ) where

import           Control.Exception       (SomeException, displayException, try)
import           Control.Monad           (forM)
import           Data.Char               (isAlphaNum)
import           Data.List               (sort, sortOn)
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Data.Set                (Set)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.Directory        (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import           System.FilePath         (takeExtension, (</>))

import GHC.Data.FastString       (unpackFS)
import GHC.Iface.Ext.Binary      (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils       (hieTypeToIface, recoverFullType)
import GHC.Types.Avail           (availNames)
import GHC.Types.Name            (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache      (initNameCache)
import GHC.Types.Name.Occurrence (isDataOcc, isTcOcc, occNameString)
import GHC.Types.SrcLoc          (RealSrcSpan, containsSpan, srcSpanStartLine)
import GHC.Unit.Module           (moduleName, moduleNameString, moduleUnit)
import GHC.Unit.Types            (unitString)
import GHC.Utils.Outputable      (ppr, showSDocUnsafe)

-- | How a name is used at one occurrence.
data RefKind
  = RefValue
  -- ^ a function or value
  | RefBuild
  -- ^ a constructor in expression position
  | RefMatch
  -- ^ a constructor in pattern position
  | RefType
  -- ^ a type or class
  deriving (Eq, Ord, Show)

-- | One resolved reference: the module and package unit that define the
-- name, the name, how it is used, and whether its type at this
-- occurrence mentions an effect monad (IO, MonadIO, MonadUnliftIO, IOE
-- or Eff).
data HieRef = HieRef
  { hrModule    :: Text
  , hrUnit      :: Text
  , hrName      :: Text
  , hrKind      :: RefKind
  , hrEffectful :: Bool
  }
  deriving (Eq, Ord, Show)

-- | A top-level value binding and the references inside its span,
-- deduplicated, without the binding's own name.
data HieBinding = HieBinding
  { hbName :: Text
  , hbLine :: Int
  , hbRefs :: [HieRef]
  }
  deriving (Show)

data HieModule = HieModule
  { hmModule       :: Text
  , hmUnit         :: Text
  , hmSource       :: FilePath
  , hmHiePath      :: FilePath
  , hmExports      :: [Text]
  , hmConstructors :: [(Text, [Text])]
  -- ^ (type name, constructors) for every data declaration
  , hmBindings     :: [HieBinding]
  }
  deriving (Show)

-- | Every .hie file under a directory, sorted. Symbolic links are not
-- followed.
findHieFiles :: FilePath -> IO [FilePath]
findHieFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir then pure [] else sort <$> walkDir root
  where
    walkDir dir = do
      entries <- listDirectory dir
      fmap concat . forM entries $ \name -> do
        let p = dir </> name
        link <- pathIsSymbolicLink p
        sub  <- doesDirectoryExist p
        file <- doesFileExist p
        if | link -> pure []
           | sub -> walkDir p
           | file && takeExtension p == ".hie" -> pure [p]
           | otherwise -> pure []

-- | Read every .hie file under a directory. A file that cannot be read
-- (another GHC's format, truncated, not a HIE file) contributes a
-- warning and nothing else. When two files describe the same module,
-- the first in path order is kept.
readHieDir :: FilePath -> IO ([HieModule], [Text])
readHieDir dir = do
  paths <- findHieFiles dir
  nc <- initNameCache 'z' []
  results <- forM paths $ \p -> do
    r <- try (readHieFile nc p) :: IO (Either SomeException HieFileResult)
    pure $ case r of
      Left e  -> Left (T.pack p <> ": " <> T.pack (displayException e))
      Right h -> Right (toModel p (hie_file_result h))
  let mods = Map.elems (Map.fromListWith (\_ first -> first)
               [ (hmModule m, m) | Right m <- results ])
  pure (sortOn hmHiePath mods, [ w | Left w <- results ])

-- | One occurrence of an identifier in the tree: its span, the name,
-- the contexts it occurs in, its type, and whether it sits under a
-- pattern node.
data Occ = Occ RealSrcSpan Name (Set ContextInfo) (Maybe TypeIndex) Bool

toModel :: FilePath -> HieFile -> HieModule
toModel hiePath hf =
  let self     = hie_module hf
      selfName = T.pack (moduleNameString (moduleName self))
      occs     = concatMap (walk False) (Map.elems (getAsts (hie_asts hf)))
      bindings =
        [ HieBinding (nameText n) (srcSpanStartLine sp) (refsWithin n sp)
        | Occ _ n info _ _ <- occs
        , Just sp <- [topBindingSpan info]
        , not (isGenerated (nameText n))
        ]
      refsWithin owner sp =
        Set.toList $ Set.fromList
          [ ref
          | Occ s n info ty inPat <- occs
          , Set.member Use info
          , containsSpan sp s
          , n /= owner
          , Just ref <- [toRef (typeMentionsEffect ty) inPat n]
          ]
      typeMentionsEffect = \case
        Nothing -> False
        Just ix ->
          let rendered = showSDocUnsafe (ppr (hieTypeToIface (recoverFullType ix (hie_types hf))))
              tokens   = words (map (\c -> if isAlphaNum c || c == '_' then c else ' ') rendered)
          in any (`elem` effectTokens) tokens
      dataDecls = [ (nameText n, sp) | Occ _ n info _ _ <- occs, Just sp <- [declSpan DataDec info] ]
      conDecls  = [ (nameText n, s)  | Occ s n info _ _ <- occs, isDecl ConDec info ]
      constructors =
        [ (t, Set.toList (Set.fromList [ c | (c, cs) <- conDecls, containsSpan sp cs ]))
        | (t, sp) <- dedupe dataDecls
        ]
  in HieModule
    { hmModule       = selfName
    , hmUnit         = T.pack (unitString (moduleUnit self))
    , hmSource       = hie_hs_file hf
    , hmHiePath      = hiePath
    , hmExports      = Set.toList (Set.fromList (map nameText (concatMap availNames (hie_exports hf))))
    , hmConstructors = constructors
    , hmBindings     = dedupeBindings bindings
    }
  where
    dedupe = Map.toList . Map.fromListWith (\_ first -> first)
    dedupeBindings bs =
      Map.elems (Map.fromListWith (\_ first -> first) [ ((hbName b, hbLine b), b) | b <- bs ])

-- | Type constructors whose presence in a type marks an effectful
-- computation.
effectTokens :: [String]
effectTokens = ["IO", "MonadIO", "MonadUnliftIO", "IOE", "Eff"]

-- | Compiler-generated bindings: record selectors ($sel:field:Type) and
-- the like.
isGenerated :: Text -> Bool
isGenerated = T.isPrefixOf "$"

-- | Every identifier occurrence in a subtree, with whether it sits under
-- a pattern node.
walk :: Bool -> HieAST TypeIndex -> [Occ]
walk inPat node =
  let infos  = Map.elems (getSourcedNodeInfo (sourcedNodeInfo node))
      isPat  = any (any ((== "Pat") . unpackFS . nodeAnnotType) . Set.toList . nodeAnnotations) infos
      here   = inPat || isPat
      idents =
        [ Occ (nodeSpan node) n (identInfo d) (identType d) here
        | ni <- infos
        , (Right n, d) <- Map.toList (nodeIdentifiers ni)
        ]
  in idents ++ concatMap (walk here) (nodeChildren node)

-- | The span of the whole binding, for a module-level value binding.
topBindingSpan :: Set ContextInfo -> Maybe RealSrcSpan
topBindingSpan info =
  case [ sp | ValBind RegularBind ModuleScope (Just sp) <- Set.toList info ] of
    (sp : _) -> Just sp
    []       -> Nothing

declSpan :: DeclType -> Set ContextInfo -> Maybe RealSrcSpan
declSpan want info =
  case [ sp | Decl d (Just sp) <- Set.toList info, d == want ] of
    (sp : _) -> Just sp
    []       -> Nothing

isDecl :: DeclType -> Set ContextInfo -> Bool
isDecl want info = or [ d == want | Decl d _ <- Set.toList info ]

-- | A reference to a name that has a defining module. Local names (let
-- and lambda bound) have none and are dropped.
toRef :: Bool -> Bool -> Name -> Maybe HieRef
toRef effectful inPat n = case nameModule_maybe n of
  Nothing -> Nothing
  Just m  ->
    let occ  = nameOccName n
        kind
          | isDataOcc occ = if inPat then RefMatch else RefBuild
          | isTcOcc occ   = RefType
          | otherwise     = RefValue
    in Just HieRef
         { hrModule    = T.pack (moduleNameString (moduleName m))
         , hrUnit      = T.pack (unitString (moduleUnit m))
         , hrName      = nameText n
         , hrKind      = kind
         , hrEffectful = effectful
         }

nameText :: Name -> Text
nameText = T.pack . occNameString . nameOccName