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
  <> renderImports    chaseImports
  <> renderConstants  chaseConstants
  <> renderTopologies chaseTopologies
  <> renderDataDecls  chaseDataDecls
  <> renderSignaturesWithInvariants chaseSignatures chaseInvariants
  <> renderDecisions  chaseDecisions
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

renderImports :: [Text] -> [Text]
renderImports [] = []
renderImports imps =
  let collapsed = filter (not . T.null) (map collapseImport imps)
  in [ "%uses " <> i | i <- collapsed ] <> [""]

-- | Collapse a possibly multi-line import to one line, drop the explicit
-- name list, drop the leading "import " keyword. The LLM does not need
-- to know that you imported (sort, nub) specifically.
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
renderDataDecls ds = concatMap one ds <> [""]
  where
    one ChaseDataDecl{..} = [cddVerbatim, ""]

renderSignaturesWithInvariants :: [Signature] -> [Invariant] -> [Text]
renderSignaturesWithInvariants sigs invs =
  concatMap (renderSig invMap) sigs
  where
    invMap = [(invFunction i, i) | i <- invs]

    renderSig m Signature{..} =
      let attached = lookup sigName m
          extra = case attached of
            Nothing -> []
            Just Invariant{..} ->
              [ "  ! " <> l | l <- invLines ]
              <> case invBodyHint of
                   Nothing -> []
                   Just h  -> ["  " <> h]
      in [sigVerbatim] <> extra <> [""]

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

renderParseErrors :: [Text] -> [Text]
renderParseErrors [] = []
renderParseErrors es = "" : [ "%parse_error " <> e | e <- es ]