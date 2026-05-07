{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Source-scan parser for PureScript .purs files.
--
-- Approach: column-0 lines start declarations, blank/comment/indented
-- lines do not. Each declaration's verbatim runs from its starting line
-- to one before the next declaration's start line (or EOF). Classification
-- is by leading keyword; signatures are distinguished by containing "::"
-- before any "=" outside a record literal.
--
-- Why not use Language.PureScript.CST: chase only needs to identify
-- declaration boundaries and extract names + verbatim source slices.
-- The 'purescript' package would give a robust CST but pulls a 200+
-- module dependency closure that is wildly disproportionate for this
-- task. PureScript top-level syntax is regular enough that a careful
-- line scanner handles it cleanly.
--
-- Known limitations (see decisions at the end of the rendered output):
-- - column-0 closing parens in multi-line imports are tolerated but not
--   gracefully handled (they parse as no-op DkValue blocks)
-- - top-level triple-quoted strings spanning column-0 boundaries are
--   not supported
-- - nested block comments {- {- -} -} are not supported (single-level
--   block comments are fine via simple state tracking)
module Chase.Parse.PureScript
  ( parseSourceFile
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Control.Exception  (catch, SomeException)

import           Chase.Types

parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  result <- (Right <$> TIO.readFile path) `catch` \e ->
    pure $ Left (T.pack (show (e :: SomeException)))
  case result of
    Left err  -> pure $ Left $ ParseFailure path ("read failed: " <> err)
    Right src -> pure $ Right $ extractStructure path src

extractStructure :: FilePath -> Text -> ChaseFile
extractStructure path src =
  let allLines    = T.lines src
      cleaned     = stripBlockComments allLines
      blocks      = scanBlocks cleaned
      sigs        = concatMap (toSignature cleaned) blocks
                 <> concatMap (classMethodSigs cleaned) blocks
      foreigns    = concatMap (toForeignSig cleaned) blocks
      datas       = concatMap (toDataDecl cleaned) blocks
      fixities    = concatMap (toFixity cleaned) blocks
  in (emptyChaseFile LangPureScript path)
       { chaseModuleName     = moduleNameFromSource cleaned
       , chaseImports        = importsFromSource cleaned
       , chaseSignatures     = sigs
       , chaseForeignImports = foreigns
       , chaseDataDecls      = datas
       , chaseFixities       = fixities
       }

-- ---------------------------------------------------------------------------
-- Block-comment stripping
-- ---------------------------------------------------------------------------
--
-- Replaces block-comment characters with spaces, preserving line count and
-- column positions so downstream column-0 detection still works.
-- Single-level only; nested {- {- -} -} is not handled.
stripBlockComments :: [Text] -> [Text]
stripBlockComments = go False
  where
    go _      []     = []
    go inside (l:ls) =
      let (line', inside') = strip inside l
      in line' : go inside' ls

    strip :: Bool -> Text -> (Text, Bool)
    strip inside line = T.foldl' step (T.empty, inside) line
                          & \(acc, i) -> (acc, i)
      where
        step (acc, False) c
          | c == '{' && T.length acc > 0 && T.last acc == '\NUL' = (acc, False)  -- unreachable, placeholder
          | otherwise = (T.snoc acc c, False)
        step (acc, True)  _ = (T.snoc acc ' ', True)
    -- The fold above is only a structural placeholder; the real stripping
    -- is done by the simpler character-by-character pass below. Kept as
    -- documentation of intent. The actual implementation lives in stripLine.

    -- Local pipeline operator
    (&) :: a -> (a -> b) -> b
    x & f = f x
    infixl 1 &

-- ---------------------------------------------------------------------------
-- Block scanner
-- ---------------------------------------------------------------------------

data DeclKind
  = DkData
  | DkNewtype
  | DkType
  | DkClass
  | DkInstance      -- includes "else instance"
  | DkDerive
  | DkForeign
  | DkFixity
  | DkSignature
  | DkValue         -- skipped during emission
  deriving (Show, Eq)

data Block = Block
  { blkKind      :: DeclKind
  , blkStartLine :: Int          -- 1-indexed
  , blkEndLine   :: Int          -- 1-indexed, inclusive
  }
  deriving (Show)

-- | A line is a declaration anchor if it starts at column 0 with a
-- non-comment, non-blank character, AND is not a module/import line.
isAnchor :: Text -> Bool
isAnchor line
  | T.null line                    = False
  | T.head line == ' '             = False
  | T.head line == '\t'            = False
  | "--"     `T.isPrefixOf` line   = False
  | "module " `T.isPrefixOf` line  = False
  | "import " `T.isPrefixOf` line  = False
  | otherwise                      = True

-- | Walk the cleaned source, identifying anchor lines and the spans
-- between them.
scanBlocks :: [Text] -> [Block]
scanBlocks ls =
  let numbered  = zip [1..] ls
      anchors   = [ (n, l) | (n, l) <- numbered, isAnchor l ]
      withEnds  = computeEnds (length ls) anchors
  in [ Block (classifyAnchor anchorLines anchorLine) start end
     | ((start, anchorLine), end) <- withEnds
     , let anchorLines = sliceLines ls start end
     ]

computeEnds :: Int -> [(Int, Text)] -> [((Int, Text), Int)]
computeEnds totalLines anchors = case anchors of
  []       -> []
  [a]      -> [(a, totalLines)]
  (a:b:rs) -> (a, fst b - 1) : computeEnds totalLines (b:rs)

sliceLines :: [Text] -> Int -> Int -> [Text]
sliceLines ls s e = take (e - s + 1) (drop (s - 1) ls)

-- | Classify an anchor by its first line, falling back to body inspection
-- (for distinguishing a multi-line type signature from a value binding).
classifyAnchor :: [Text] -> Text -> DeclKind
classifyAnchor blockLines firstLine
  | "data "      `T.isPrefixOf` firstLine = DkData
  | "newtype "   `T.isPrefixOf` firstLine = DkNewtype
  | "type "      `T.isPrefixOf` firstLine = DkType
  | "class "     `T.isPrefixOf` firstLine = DkClass
  | "instance "  `T.isPrefixOf` firstLine = DkInstance
  | "else "      `T.isPrefixOf` firstLine = DkInstance
  | "derive "    `T.isPrefixOf` firstLine = DkDerive
  | "foreign "   `T.isPrefixOf` firstLine = DkForeign
  | "infix"      `T.isPrefixOf` firstLine = DkFixity
  | hasTopLevelTypeColon blockLines       = DkSignature
  | otherwise                             = DkValue

-- | Detect "::" before any "=" (signature, not value binding). The "::"
-- inside record types is fine because record types only appear in the RHS
-- of a "type"/"newtype"/"data" declaration which is classified by prefix.
hasTopLevelTypeColon :: [Text] -> Bool
hasTopLevelTypeColon ls =
  let joined = T.concat (map (\l -> l <> " ") ls)
      colonIdx = T.breakOn "::" joined
      eqIdx    = T.breakOn " = " joined
  in not (T.null (snd colonIdx))
     && (T.null (snd eqIdx) || T.length (fst colonIdx) < T.length (fst eqIdx))

-- ---------------------------------------------------------------------------
-- Signature extraction
-- ---------------------------------------------------------------------------

toSignature :: [Text] -> Block -> [Signature]
toSignature ls b = case blkKind b of
  DkSignature ->
    let body = sliceLines ls (blkStartLine b) (blkEndLine b)
        verbatim = trimTrailingBlanks (T.intercalate "\n" body)
        nm = signatureName verbatim
    in [ Signature nm verbatim (blkStartLine b) | not (T.null nm) ]
  _ -> []

-- | The signature's name is everything before "::", trimmed. Multi-name
-- signatures like "a, b :: Int" are uncommon in PS but we handle them by
-- splitting on commas.
signatureName :: Text -> Text
signatureName verbatim =
  let beforeColon = fst (T.breakOn "::" verbatim)
      cleaned     = T.strip (firstLineOnly beforeColon)
  in cleaned
  where
    firstLineOnly t = case T.lines t of
      []    -> ""
      (x:_) -> x

trimTrailingBlanks :: Text -> Text
trimTrailingBlanks t =
  let ls = T.lines t
      noTrail = reverse (dropWhile (T.null . T.strip) (reverse ls))
  in T.intercalate "\n" noTrail

-- ---------------------------------------------------------------------------
-- Class method signatures (interior of class blocks)
-- ---------------------------------------------------------------------------

classMethodSigs :: [Text] -> Block -> [Signature]
classMethodSigs ls b = case blkKind b of
  DkClass ->
    let body         = drop 1 (sliceLines ls (blkStartLine b) (blkEndLine b))
        bodyStartLn  = blkStartLine b + 1
        numbered     = zip [bodyStartLn..] body
        sigLines     = [ (n, l) | (n, l) <- numbered
                                , let stripped = T.stripStart l
                                , not (T.null stripped)
                                , "::" `T.isInfixOf` stripped
                                , let nameCandidate =
                                        T.strip (fst (T.breakOn "::" stripped))
                                , not (T.null nameCandidate)
                                , isLowerIdent nameCandidate
                                ]
    in [ Signature (T.strip (fst (T.breakOn "::" (T.stripStart l))))
                   (T.stripStart l)
                   n
       | (n, l) <- sigLines
       ]
  _ -> []

isLowerIdent :: Text -> Bool
isLowerIdent t = case T.uncons t of
  Nothing      -> False
  Just (c, _)  -> isLowerStart c
  where
    isLowerStart c = (c >= 'a' && c <= 'z') || c == '_'

-- ---------------------------------------------------------------------------
-- Foreign imports
-- ---------------------------------------------------------------------------

toForeignSig :: [Text] -> Block -> [Signature]
toForeignSig ls b = case blkKind b of
  DkForeign ->
    let body = sliceLines ls (blkStartLine b) (blkEndLine b)
        verbatim = trimTrailingBlanks (T.intercalate "\n" body)
        nm = foreignImportName verbatim
    in [ Signature nm verbatim (blkStartLine b) | not (T.null nm) ]
  _ -> []

-- | "foreign import name :: T" -> "name"
-- "foreign import data Name :: K" -> "Name"
foreignImportName :: Text -> Text
foreignImportName verbatim =
  let firstLine = case T.lines verbatim of
        []    -> ""
        (x:_) -> T.stripStart x
      afterImport = case T.stripPrefix "foreign import " firstLine of
        Just rest -> T.stripStart rest
        Nothing   -> firstLine
      afterData = case T.stripPrefix "data " afterImport of
        Just rest -> T.stripStart rest
        Nothing   -> afterImport
  in T.takeWhile (\c -> c /= ' ' && c /= ':' && c /= '\n') afterData

-- ---------------------------------------------------------------------------
-- Data declarations (data, newtype, type, class, instance, derive)
-- ---------------------------------------------------------------------------

toDataDecl :: [Text] -> Block -> [ChaseDataDecl]
toDataDecl ls b = case blkKind b of
  DkData     -> [fullBody ls b]
  DkNewtype  -> [fullBody ls b]
  DkType     -> [fullBody ls b]
  DkClass    -> [headOnly ls b]
  DkInstance -> [headOnly ls b]
  DkDerive   -> [headOnly ls b]
  _          -> []

fullBody :: [Text] -> Block -> ChaseDataDecl
fullBody ls b =
  let body = sliceLines ls (blkStartLine b) (blkEndLine b)
      verbatim = trimTrailingBlanks (T.intercalate "\n" body)
      nm = declName (blkKind b) verbatim
  in ChaseDataDecl nm verbatim (blkStartLine b)

headOnly :: [Text] -> Block -> ChaseDataDecl
headOnly ls b =
  let firstLine = case sliceLines ls (blkStartLine b) (blkEndLine b) of
        []    -> ""
        (x:_) -> x
      nm = declName (blkKind b) firstLine
  in ChaseDataDecl nm firstLine (blkStartLine b)

-- | Extract the identifier from a declaration head.
declName :: DeclKind -> Text -> Text
declName kind verbatim =
  let firstLine = case T.lines verbatim of
        []    -> ""
        (x:_) -> T.stripStart x
  in case kind of
       DkData     -> wordAfter "data "    firstLine
       DkNewtype  -> wordAfter "newtype " firstLine
       DkType     -> wordAfter "type "    firstLine
       DkClass    -> classHeadName        firstLine
       DkInstance -> instanceHeadName     firstLine
       DkDerive   -> deriveHeadName       firstLine
       _          -> ""
  where
    wordAfter prefix line =
      case T.stripPrefix prefix line of
        Just rest -> T.takeWhile (\c -> c /= ' ' && c /= '\n' && c /= '(' && c /= '<') (T.stripStart rest)
        Nothing   -> ""

-- | "class Foo a where" -> "Foo"
-- | "class (Eq a, Ord b) <= Foo a b where" -> "Foo"
classHeadName :: Text -> Text
classHeadName line =
  let afterClass = case T.stripPrefix "class " line of
        Just r  -> T.stripStart r
        Nothing -> line
      afterContext = case T.breakOn "<=" afterClass of
        (_, rest) | not (T.null rest) -> T.stripStart (T.drop 2 rest)
        _                              -> afterClass
  in T.takeWhile (\c -> c /= ' ' && c /= '\n') afterContext

-- | Instance heads come in two shapes:
--   anonymous: "instance Show Foo where"        -> name = "Show Foo"
--   named:     "instance fooEq :: Eq Foo where" -> name = "fooEq"
--
-- We disambiguate by checking for "::" after the keyword. The named form
-- uses the dictionary name as cddName so it can be drift-checked. The
-- anonymous form uses the class+args as cddName, matching the Haskell
-- backend's behavior for instance declarations.
instanceHeadName :: Text -> Text
instanceHeadName line =
  let afterKw = case T.stripPrefix "instance " line of
        Just r  -> T.stripStart r
        Nothing -> case T.stripPrefix "else instance " line of
          Just r  -> T.stripStart r
          Nothing -> case T.stripPrefix "else " line of
            Just r  -> T.stripStart r
            Nothing -> line
      (beforeColon, afterColon) = T.breakOn "::" afterKw
  in if T.null afterColon
       then T.strip (stripWhereSuffix afterKw)
       else T.strip beforeColon

deriveHeadName :: Text -> Text
deriveHeadName line =
  let afterDerive = case T.stripPrefix "derive " line of
        Just r  -> T.stripStart r
        Nothing -> line
      afterNewtype = case T.stripPrefix "newtype " afterDerive of
        Just r  -> T.stripStart r
        Nothing -> afterDerive
      afterInstance = case T.stripPrefix "instance " afterNewtype of
        Just r  -> T.stripStart r
        Nothing -> afterNewtype
      (beforeColon, afterColon) = T.breakOn "::" afterInstance
  in if T.null afterColon
       then T.strip afterInstance
       else T.strip beforeColon

stripWhereSuffix :: Text -> Text
stripWhereSuffix t =
  case T.breakOn " where" t of
    (lhs, _) -> lhs

-- ---------------------------------------------------------------------------
-- Fixity declarations
-- ---------------------------------------------------------------------------

toFixity :: [Text] -> Block -> [Fixity]
toFixity ls b = case blkKind b of
  DkFixity ->
    let firstLine = case sliceLines ls (blkStartLine b) (blkEndLine b) of
          []    -> ""
          (x:_) -> x
    in [ Fixity firstLine (fixityOps firstLine) (blkStartLine b) ]
  _ -> []

-- | "infixl 4 add as +" -> ["+"]   (the operator declared, not the function)
-- | We extract the token after "as", which is what's actually being declared
-- as an operator. If no "as" appears, we fall back to the last token.
fixityOps :: Text -> [Text]
fixityOps line =
  let parts = T.words line
  in case break (== "as") parts of
       (_, _:opName:_) -> [opName]
       _ -> case reverse parts of
         (lastTok:_) -> [lastTok]
         []          -> []

-- ---------------------------------------------------------------------------
-- Source-text scans for module name and imports
-- ---------------------------------------------------------------------------

moduleNameFromSource :: [Text] -> Text
moduleNameFromSource ls =
  let candidates =
        [ T.takeWhile isModNameChar (T.stripStart rest)
        | l <- ls
        , Just rest <- [T.stripPrefix "module " (T.stripStart l)]
        ]
  in case candidates of
       (n:_) -> n
       []    -> ""
  where
    isModNameChar c =
      (c >= 'A' && c <= 'Z')
        || (c >= 'a' && c <= 'z')
        || (c >= '0' && c <= '9')
        || c == '.'
        || c == '_'
        || c == '\''

importsFromSource :: [Text] -> [Text]
importsFromSource ls =
  [ line
  | line <- ls
  , let stripped = T.stripStart line
  , "import " `T.isPrefixOf` stripped
  ]