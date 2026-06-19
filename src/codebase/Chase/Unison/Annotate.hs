{-# LANGUAGE OverloadedStrings #-}

module Chase.Unison.Annotate
  ( AnnotatedResult (..)
  , renderAnnotated
  ) where

import Chase.Types
import Chase.Unison.Extract (SkelEntry (..), SkelKind (..), UnisonSkeleton (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data AnnotatedResult = AnnotatedResult
  { arText          :: Text
  , arDriftWarnings :: [Text]   -- ^ annotated names the extractor never saw
  }

-- | Render a Unison skeleton in the chase format with annotations merged in.
-- Invariants attach to entries by best (least-qualified) name; decisions and
-- open issues render as trailing blocks. An empty ModuleAnnotations yields
-- structure-only output, which is exactly what you hand to an LLM to draft
-- annotations from.
renderAnnotated :: UnisonSkeleton -> ModuleAnnotations -> AnnotatedResult
renderAnnotated usk anns = AnnotatedResult rendered drift
  where
    invByName :: Map Text Invariant
    invByName = Map.fromList [ (invFunction i, i) | i <- annInvariants anns ]

    nameSet :: Set.Set Text
    nameSet = Set.fromList [ n | e <- uskEntries usk, n <- seNames e ]

    rendered =
      T.unlines $
        header
          ++ concatMap renderEntry (uskEntries usk)
          ++ concatMap renderDecision (annDecisions anns)
          ++ concatMap renderOpenIssue (annOpenIssues anns)

    header =
      [ "%codebase " <> uskProject usk <> "/" <> uskBranch usk
      , "%root " <> (if T.null (uskRoot usk) then "(whole branch)" else uskRoot usk)
      , "%defs " <> T.pack (show (length (uskEntries usk)))
      , ""
      , "# chase-unison bundle: a structural skeleton of a Unison codebase,"
      , "# one entry per definition, deduplicated by content hash."
      , "#   name : signature   ~hash     term; ~hash is the content address"
      , "#   type[Tag] name     ~hash     type, followed by its declaration"
      , "#     fields: a, b, c            record fields (generated accessors elided)"
      , "#   > aka: other.name            another name for the same hash"
      , "#   ! ...                        behavioral invariant (from annotations)"
      , "#   > consumed by: ...           downstream dependent (from annotations)"
      , "# %decision / %open_issue blocks at the end are architectural notes."
      , "# Lines from annotations appear only when an annotations file is supplied."
      , ""
      ]

    renderEntry e = body ++ invLinesFor best ++ akaLines ++ [""]
      where
        best = bestName (seNames e)
        sh   = "~" <> T.take 10 (T.dropWhile (== '#') (seHash e))
        body = case seKind e of
          SkelTerm sig -> [best <> " : " <> sig <> "   " <> sh]
          SkelType tag decl flds ->
            ("type[" <> tag <> "] " <> best <> "   " <> sh)
              : map ("  " <>) (declLines decl)
              ++ fieldLines flds
        declLines d   = if T.null d then [] else T.lines d
        fieldLines [] = []
        fieldLines fs = ["  fields: " <> T.intercalate ", " fs]
        aka      = filter (/= best) (seNames e)
        akaLines = ["  > aka: " <> T.intercalate ", " aka | not (null aka)]

    invLinesFor name = case Map.lookup name invByName of
      Nothing -> []
      Just i  ->
        map ("  ! " <>) (invLines i)
          ++ map ("  > consumed by: " <>) (invConsumes i)
          ++ maybe [] (\h -> ["  body: " <> h]) (invBodyHint i)

    drift =
      [ "invariant references unknown definition: " <> invFunction i
      | i <- annInvariants anns, not (Set.member (invFunction i) nameSet) ]
      ++
      [ "decision " <> decName d <> " affects unknown definition: " <> a
      | d <- annDecisions anns, a <- decAffects d, not (Set.member a nameSet) ]
      ++
      [ "open issue " <> oiName o <> " affects unknown definition: " <> a
      | o <- annOpenIssues anns, a <- oiAffects o, not (Set.member a nameSet) ]

bestName :: [Text] -> Text
bestName (n : _) = n
bestName []      = "(anonymous)"

renderDecision :: Decision -> [Text]
renderDecision d =
  [ "%decision " <> decName d
  , "  what:    " <> decWhat d
  , "  why:     " <> decWhy d
  ]
    ++ [ "  affects: " <> T.intercalate ", " (decAffects d) | not (null (decAffects d)) ]
    ++ [""]

renderOpenIssue :: OpenIssue -> [Text]
renderOpenIssue o =
  [ "%open_issue " <> oiName o
  , "  what:     " <> oiWhat o
  , "  why:      " <> oiWhy o
  ]
    ++ [ "  blocking: " <> T.intercalate ", " (oiBlocking o) | not (null (oiBlocking o)) ]
    ++ [ "  affects:  " <> T.intercalate ", " (oiAffects o)  | not (null (oiAffects o)) ]
    ++ [""]