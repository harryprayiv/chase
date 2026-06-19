{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chase.Render
  ( renderChaseFile
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import Chase.Types

renderChaseFile :: ChaseFile -> Text
renderChaseFile ChaseFile{..} = T.unlines $
     header
  <> renderExtensions chaseExtensions
  <> renderFixities   chaseFixities
  <> renderImports    chaseImports
  <> renderConstants  chaseConstants
  <> renderTopologies chaseTopologies
  <> renderDataDecls  chaseDataDecls
  <> renderForeignImports chaseForeignImports chaseInvariants chaseTestCoverage
  <> renderPatternsWithInvariants chasePatterns chaseInvariants chaseTestCoverage
  <> renderSignaturesWithInvariants chaseSignatures chaseInvariants chaseTestCoverage
  <> renderDecisions  chaseDecisions
  <> renderOpenIssues chaseOpenIssues
  <> renderParseErrors chaseParseErrors
  where
    header =
      [ "%file " <> T.pack chaseSourcePath
      , "%mod "  <> chaseModuleName
      , ""
      ]

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

renderTopologies :: [Topology] -> [Text]
renderTopologies [] = []
renderTopologies tops = concatMap renderOne tops <> [""]
  where
    renderOne Topology{..} =
      ( "%machine " <> topName
        <> " vertices=" <> T.intercalate "," topVertices )
      : [ "%topology " <> from <> " -> {" <> T.intercalate ", " tos <> "}"
        | (from, tos) <- topTransitions
        ]

renderDataDecls :: [ChaseDataDecl] -> [Text]
renderDataDecls [] = []
renderDataDecls ds = concatMap one ds
  where
    one ChaseDataDecl{..} = [cddVerbatim, ""]

renderForeignImports
  :: [Signature]
  -> [Invariant]
  -> Maybe [(Text, [TestRef])]
  -> [Text]
renderForeignImports [] _ _ = []
renderForeignImports sigs invs mcov =
  "%foreign" : concatMap (renderSig invMap mcov) sigs
  where
    invMap = [(invFunction i, i) | i <- invs]

renderPatternsWithInvariants
  :: [Pattern]
  -> [Invariant]
  -> Maybe [(Text, [TestRef])]
  -> [Text]
renderPatternsWithInvariants [] _ _ = []
renderPatternsWithInvariants pats invs mcov =
  concatMap renderPat pats
  where
    invMap = [(invFunction i, i) | i <- invs]
    renderPat Pattern{..} =
      let attached = lookup patName invMap
          extra = case attached of
            Nothing -> []
            Just Invariant{..} ->
                 [ "  ! " <> l                | l <- invLines    ]
              <> [ "  > consumed by: " <> c   | c <- invConsumes ]
              <> case invBodyHint of
                   Nothing -> []
                   Just h  -> ["  " <> h]
          covLine = renderCoverage patName mcov
      in [patVerbatim] <> extra <> covLine <> [""]

renderSignaturesWithInvariants
  :: [Signature]
  -> [Invariant]
  -> Maybe [(Text, [TestRef])]
  -> [Text]
renderSignaturesWithInvariants sigs invs mcov =
  concatMap (renderSig invMap mcov) sigs
  where
    invMap = [(invFunction i, i) | i <- invs]

renderSig
  :: [(Text, Invariant)]
  -> Maybe [(Text, [TestRef])]
  -> Signature
  -> [Text]
renderSig invMap mcov Signature{..} =
  let attached = lookup sigName invMap
      extra = case attached of
        Nothing -> []
        Just Invariant{..} ->
             [ "  ! " <> l                | l <- invLines    ]
          <> [ "  > consumed by: " <> c   | c <- invConsumes ]
          <> case invBodyHint of
               Nothing -> []
               Just h  -> ["  " <> h]
      covLine = renderCoverage sigName mcov
  in [sigVerbatim] <> extra <> covLine <> [""]

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