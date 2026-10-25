{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chase.Render
  ( renderChaseFile
  , renderDerivedTopologies
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import Chase.Types

renderChaseFile :: ChaseFile -> Text
renderChaseFile cf@ChaseFile{..} = T.unlines $
     header
  <> renderExtensions chaseExtensions
  <> renderFixities   chaseFixities
  <> renderImports    chaseImports
  <> renderConstants  chaseConstants
  <> renderConstCandidates cf
  <> renderTopologies chaseTopologies
  <> renderAbstract   chaseDerived
  <> renderDataDecls  chaseDataDecls
  <> renderForeignImports chaseForeignImports extras
  <> renderPatternsWithInvariants chasePatterns extras
  <> renderSignaturesWithInvariants chaseSignatures extras
  <> renderDecisions  chaseDecisions
  <> renderOpenIssues chaseOpenIssues
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
      }

-- | Everything that hangs off a name under its signature.
data SigExtras = SigExtras
  { seInvariants :: [(Text, Invariant)]
  , seCoverage   :: Maybe [(Text, [TestRef])]
  , seCallers    :: Maybe [(Text, [TestRef])]
  , seDerived    :: Maybe Derived
  }

renderExtensions :: [Text] -> [Text]
renderExtensions [] = []
renderExtensions xs = ["%ext " <> T.intercalate ", " xs, ""]

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
-- and consumers, then derived facts, then callers, then coverage.
extraLines :: SigExtras -> Text -> [Text]
extraLines SigExtras{..} name =
     invariantLines
  <> derivedLines
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
    callerLines = case seCallers of
      Nothing -> []
      Just cs -> case lookup name cs of
        Nothing   -> []
        Just []   -> ["  ~ no callers in scanned roots"]
        Just refs -> ["  ~ called by: " <> T.intercalate ", " (map renderTestRef refs)]
    lookupAll k kvs = concat [ v | (k', v) <- kvs, k' == k ]

renderCoverage :: Text -> Maybe [(Text, [TestRef])] -> [Text]
renderCoverage _    Nothing       = []
renderCoverage name (Just covMap) =
  case lookup name covMap of
    Nothing   -> []
    Just []   -> ["  ? no test references"]
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