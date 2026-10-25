{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chase.Render
  ( renderChaseFile
  , renderDerivedTopologies
  , renderConstructorUsage
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import Chase.Types

renderChaseFile :: ChaseFile -> Text
renderChaseFile cf@ChaseFile{..} = T.unlines $
     header
  <> renderExtensions chaseExtensions
  <> renderModuleWarnings chaseGhcWarnings
  <> renderFixities   chaseFixities
  <> renderImports    chaseImports
  <> renderConstants  chaseConstants
  <> renderConstCandidates cf
  <> renderTopologies chaseTopologies
  <> renderAbstract   chaseDerived
  <> renderUntested   chaseTestCoverage
  <> renderDataDecls  chaseDataDecls
  <> renderForeignImports chaseForeignImports extras
  <> renderPatternsWithInvariants chasePatterns extras
  <> renderSignaturesWithInvariants chaseSignatures extras
  <> renderDecisions  chaseDecisions
  <> renderOpenIssues chaseOpenIssues
  <> renderOpenQuestions chaseOpenQuestions
  <> renderParseErrors chaseParseErrors
  where
    header =
      [ "%file " <> T.pack chaseSourcePath
      , "%mod "  <> chaseModuleName
      , ""
      ]
    extras = SigExtras
      { seInvariants = [(invFunction i, i) | i <- chaseInvariants]
      , seCoverage   = chaseTestCoverage
      , seCallers    = chaseCallers
      , seDerived    = chaseDerived
      , seHie        = chaseHie
      , seWarnings   = chaseGhcWarnings
      }

-- | Everything that hangs off a name under its signature.
data SigExtras = SigExtras
  { seInvariants :: [(Text, Invariant)]
  , seCoverage   :: Maybe [(Text, [TestRef])]
  , seCallers    :: Maybe [(Text, [TestRef])]
  , seDerived    :: Maybe Derived
  , seHie        :: Maybe HieFacts
  , seWarnings   :: [GhcWarning]
  }

-- | How many callers a ~ called by line names before giving a count.
callerCap :: Int
callerCap = 8

-- | How many names an %untested line carries.
untestedPerLine :: Int
untestedPerLine = 8

renderExtensions :: [Text] -> [Text]
renderExtensions [] = []
renderExtensions xs = ["%ext " <> T.intercalate ", " xs, ""]

-- | Compiler warnings that precede every signature of the file: a
-- module-level pragma, an import, a data declaration (partial fields).
renderModuleWarnings :: [GhcWarning] -> [Text]
renderModuleWarnings ws =
  case [ w | w <- ws, gwFunction w == Nothing ] of
    [] -> []
    ms -> [ "%ghc line " <> T.pack (show (gwLine w)) <> " [-" <> gwFlag w <> "] " <> gwMessage w | w <- ms ] <> [""]

renderFixities :: [Fixity] -> [Text]
renderFixities [] = []
renderFixities fs = "%fixity" : map (("  " <>) . fixVerbatim) fs <> [""]

renderImports :: [Text] -> [Text]
renderImports [] = []
renderImports imps =
  let collapsed = filter (not . T.null) (map collapseImport imps)
  in [ "%uses " <> i | i <- collapsed ] <> [""]

collapseImport :: Text -> Text
collapseImport raw =
  let oneLine  = T.unwords (T.words raw)
      chopped  = T.takeWhile (/= '(') oneLine
      stripped = T.strip chopped
  in case T.stripPrefix "import " stripped of
       Just rest -> T.strip rest
       Nothing   -> stripped

renderConstants :: [Constant] -> [Text]
renderConstants [] = []
renderConstants cs = map renderOne cs <> [""]
  where
    renderOne Constant{..} =
      let suffix = case constNotes of
            [] -> ""
            ns -> "  -- " <> T.intercalate "; " (map renderNote ns)
      in "%const " <> constName <> " = " <> constValueDoc <> suffix
    renderNote (Note t)    = t
    renderNote (BugNote t) = "BUG: " <> t

-- | Zero-argument literal bindings the annotation file does not name.
renderConstCandidates :: ChaseFile -> [Text]
renderConstCandidates ChaseFile{..} =
  case chaseDerived of
    Nothing -> []
    Just d  ->
      let cands = [ (n, v) | (n, v) <- derConstCands d
                           , n `notElem` map constName chaseConstants ]
      in if null cands
           then []
           else [ "%const_candidate " <> n <> " = " <> v | (n, v) <- cands ] <> [""]

renderTopologies :: [Topology] -> [Text]
renderTopologies [] = []
renderTopologies tops = concatMap renderOne tops <> [""]
  where
    renderOne Topology{..} =
      ( "%machine " <> topName
        <> " vertices=" <> T.intercalate "," topVertices
        <> (if null topTerminal then "" else " terminal=" <> T.intercalate "," topTerminal) )
      : [ "%topology " <> from <> " -> {" <> T.intercalate ", " tos <> "}"
        | (from, tos) <- topTransitions
        ]

renderAbstract :: Maybe Derived -> [Text]
renderAbstract Nothing = []
renderAbstract (Just d) = case derAbstract d of
  [] -> []
  as -> map one as <> [""]
  where
    one AbstractType{..} =
      "%abstract " <> atName
        <> "  hidden: " <> T.intercalate ", " atHidden
        <> (if null atMakers then "  made by: no exported function returns it"
                             else "  made by: " <> T.intercalate ", " atMakers)

-- | The names no test references, a few per line. Replaces one
-- "? no test references" line under each of them.
renderUntested :: Maybe [(Text, [TestRef])] -> [Text]
renderUntested Nothing = []
renderUntested (Just cov) =
  case [ n | (n, []) <- cov ] of
    []    -> []
    names -> [ "%untested " <> T.intercalate ", " chunk | chunk <- chunksOf untestedPerLine names ] <> [""]
  where
    chunksOf _ [] = []
    chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

renderDataDecls :: [ChaseDataDecl] -> [Text]
renderDataDecls [] = []
renderDataDecls ds = concatMap one ds
  where
    one ChaseDataDecl{..} = [cddVerbatim, ""]

renderForeignImports :: [Signature] -> SigExtras -> [Text]
renderForeignImports [] _ = []
renderForeignImports sigs extras =
  "%foreign" : concatMap (renderSig extras) sigs

renderPatternsWithInvariants :: [Pattern] -> SigExtras -> [Text]
renderPatternsWithInvariants [] _ = []
renderPatternsWithInvariants pats extras =
  concatMap renderPat pats
  where
    renderPat Pattern{..} =
      [patVerbatim] <> extraLines extras patName <> [""]

renderSignaturesWithInvariants :: [Signature] -> SigExtras -> [Text]
renderSignaturesWithInvariants sigs extras =
  concatMap (renderSig extras) sigs

renderSig :: SigExtras -> Signature -> [Text]
renderSig extras Signature{..} =
  [sigVerbatim] <> extraLines extras sigName <> [""]

-- | The lines under one name, in a fixed order: hand-written invariants
-- and consumers, then facts from the parse, then facts from the HIE
-- file, then the compiler's warnings, then callers, then coverage.
extraLines :: SigExtras -> Text -> [Text]
extraLines SigExtras{..} name =
     invariantLines
  <> derivedLines
  <> hieLines
  <> warningLines
  <> callerLines
  <> renderCoverage name seCoverage
  where
    invariantLines = case lookup name seInvariants of
      Nothing -> []
      Just Invariant{..} ->
           [ "  ! " <> l              | l <- invLines    ]
        <> [ "  > consumed by: " <> c | c <- invConsumes ]
        <> case invBodyHint of
             Nothing -> []
             Just h  -> ["  " <> h]
    derivedLines = case seDerived of
      Nothing -> []
      Just d  ->
           [ "  ~ clause: " <> c   | c <- lookupAll name (derClauses d) ]
        <> [ "  ~ fails: " <> T.intercalate ", " fs
           | let fs = lookupAll name (derFails d), not (null fs) ]
        <> [ "  ~ literals: " <> T.intercalate ", " ls
           | let ls = lookupAll name (derLiterals d), not (null ls) ]
    hieLines = case seHie of
      Nothing -> []
      Just h  ->
           [ "  ~ uses: " <> T.intercalate ", " us
           | let us = lookupAll name (hfUses h), not (null us) ]
        <> [ "  ~ unreferenced: not exported, and nothing in the HIE set references it"
           | name `elem` hfUnreferenced h ]
    warningLines =
      [ "  ~ ghc [-" <> gwFlag w <> "]: " <> gwMessage w
      | w <- seWarnings
      , gwFunction w == Just name
      ]
    callerLines = case seCallers of
      Nothing -> []
      Just cs -> case lookup name cs of
        Nothing   -> []
        Just []   -> ["  ~ no callers in scanned roots"]
        Just refs ->
          let shown = map renderTestRef (take callerCap refs)
              rest  = length refs - callerCap
              more  = if rest > 0 then [ "+" <> T.pack (show rest) <> " more" ] else []
          in ["  ~ called by: " <> T.intercalate ", " (shown <> more)]
    lookupAll k kvs = concat [ v | (k', v) <- kvs, k' == k ]

-- | The tests that reference a name. A name with none is listed in the
-- module's %untested block instead.
renderCoverage :: Text -> Maybe [(Text, [TestRef])] -> [Text]
renderCoverage _    Nothing       = []
renderCoverage name (Just covMap) =
  case lookup name covMap of
    Nothing   -> []
    Just []   -> []
    Just refs -> ["  ? tested by: "
                  <> T.intercalate ", " (map renderTestRef refs)]

renderTestRef :: TestRef -> Text
renderTestRef tr = trModule tr <> "." <> trFunction tr

renderDecisions :: [Decision] -> [Text]
renderDecisions [] = []
renderDecisions ds = concatMap one ds
  where
    one Decision{..} =
      [ "%decision " <> decName
      , "  what:    " <> decWhat
      , "  why:     " <> decWhy
      , "  affects: " <> T.intercalate ", " decAffects
      , ""
      ]

renderOpenIssues :: [OpenIssue] -> [Text]
renderOpenIssues [] = []
renderOpenIssues is = concatMap one is
  where
    one OpenIssue{..} =
      [ "%open_issue " <> oiName
      , "  what:     " <> oiWhat
      , "  why:      " <> oiWhy
      ]
      <> [ "  blocking: " <> T.intercalate ", " oiBlocking
         | not (null oiBlocking)
         ]
      <> [ "  affects:  " <> T.intercalate ", " oiAffects
         | not (null oiAffects)
         ]
      <> [""]

-- | Undecided design questions, one line each.
renderOpenQuestions :: [Text] -> [Text]
renderOpenQuestions [] = []
renderOpenQuestions qs = [ "%open_question " <> q | q <- qs ] <> [""]

renderParseErrors :: [Text] -> [Text]
renderParseErrors [] = []
renderParseErrors es = "" : [ "%parse_error " <> e | e <- es ]

-- | The run-wide derived transitions: one block per sum type that has
-- any, each edge with the clause that witnesses it. "*" on the left is a
-- clause that builds the constructor without matching one of the type.
renderDerivedTopologies :: [DerivedTopology] -> Text
renderDerivedTopologies [] = ""
renderDerivedTopologies tops = T.unlines $
  [ ""
  , "=== DERIVED TRANSITIONS ==="
  , ""
  , "# Edges read from clauses and case alternatives: a clause that matches"
  , "# constructor A of a sum type and builds constructor B of the same type"
  , "# gives A -> B. These are may-edges from syntax, not proof that the"
  , "# transition is reachable. %topology annotations are checked against"
  , "# them; the warnings are printed by the run, not written here."
  , ""
  ] <> concatMap one tops
  where
    one DerivedTopology{..} =
      ( "%derived " <> dtModule <> "." <> dtType
          <> " constructors=" <> T.intercalate "," dtConstructors )
      : [ "  " <> deFrom e <> " -> " <> deTo e <> "  via " <> deWitness e
        | e <- dtEdges
        ]
      <> [""]

-- | Constructors nothing builds, and constructors of multi-constructor
-- types nothing matches, from the HIE files.
renderConstructorUsage :: [ConstructorUsage] -> Text
renderConstructorUsage [] = ""
renderConstructorUsage cus = T.unlines $
  [ ""
  , "=== CONSTRUCTOR USAGE ==="
  , ""
  , "# From the HIE files, with names resolved. A constructor nothing builds"
  , "# is a state the program never enters; a constructor of a sum type"
  , "# nothing matches is a state nothing observes. Instance method bodies"
  , "# are not attributed to a binding, so a constructor used only inside an"
  , "# instance appears here."
  , ""
  ] <> concatMap one cus
  where
    one ConstructorUsage{..} =
         [ "%never_built " <> cuModule <> "." <> cuName <> " (" <> cuType <> ")"
             <> (if null cuMatchers then "  matched by: nothing"
                                    else "  matched by: " <> T.intercalate ", " cuMatchers)
         | null cuBuilders ]
      <> [ "%never_matched " <> cuModule <> "." <> cuName <> " (" <> cuType <> ")"
             <> "  built by: " <> T.intercalate ", " cuBuilders
         | not (null cuBuilders), null cuMatchers ]