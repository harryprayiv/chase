{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The derive pass: facts read from parsed bodies, and the checks that
-- hold hand-written annotations to them.
--
-- The language backends produce the facts (Chase.Derive.Haskell for
-- Haskell; PureScript has no derive pass yet and yields emptyDerived).
-- This module is language-neutral: it holds the shape a backend hands
-- back, builds the run-wide constructor map the backends need, merges
-- transition edges across files, attaches callers, and runs the checks.
--
-- The checks follow the obligations an Allium transition graph carries:
-- every declared edge is witnessed by a rule; a rule producing an edge
-- the graph does not declare is an error; every non-terminal state has an
-- outbound edge. Here the rules are clauses and case alternatives, the
-- graph is a %topology annotation, and all three are warnings, because
-- the derived edges are may-edges read from syntax.
--
-- This module imports only Chase.Types. Chase.Parse.Haskell imports it,
-- and Chase.Callers reaches Chase.Parse.Haskell through Chase.Coverage,
-- so an import of Chase.Callers here is a cycle. The caller index is
-- taken as the plain map it is.
module Chase.Derive
  ( Parsed (..)
  , ConMap
  , RefIndex
  , buildConMap
  , mergeTopologies
  , attachCallers
  , checkTopologyDrift
  , checkConsumesDrift
  , checkConstCandidates
  ) where

import           Data.Char       (isUpper, isLower)
import           Data.List       (nub, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set        as Set
import           Data.Set        (Set)
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Chase.Types

-- | Constructor name to the (module, type) pairs that declare it, over
-- every file parsed in the run. Two types sharing a constructor name map
-- to both; an edge is only drawn between constructors of one type.
type ConMap = Map Text (Set (Text, Text))

-- | (defining module, name) to the bindings that reference it. The same
-- type as Chase.Coverage.TestRefIndex and Chase.Callers.CallerIndex.
type RefIndex = Map (Text, Text) [TestRef]

-- | What a backend returns for one file: the skeleton, the sum types the
-- file declares, and a function that reads the derived facts once the
-- run-wide constructor map is known. The closure keeps the backend's AST
-- type out of the pipeline and avoids parsing the file twice.
data Parsed = Parsed
  { parsedFile     :: ChaseFile
  , parsedSumTypes :: [(Text, [Text])]
  -- ^ (type name, constructors) for every data type with two or more
  -- constructors
  , parsedDerive   :: ConMap -> Derived
  }

buildConMap :: [Parsed] -> ConMap
buildConMap ps =
  Map.fromListWith Set.union
    [ (c, Set.singleton (chaseModuleName (parsedFile p), t))
    | p <- ps
    , (t, cons) <- parsedSumTypes p
    , c <- cons
    ]

-- | All derived edges of the run, grouped by (module, type).
mergeTopologies :: [Parsed] -> [Derived] -> [DerivedTopology]
mergeTopologies ps ds =
  [ DerivedTopology t m cons (sortOn (\e -> (deFrom e, deTo e, deWitness e)) es)
  | p <- ps
  , let m = chaseModuleName (parsedFile p)
  , (t, cons) <- parsedSumTypes p
  , let es = nub [ e | d <- ds, e <- derEdges d, deType e == m <> "." <> t ]
  , not (null es)
  ]

-- | The callers of one name as "Module.function", deduplicated, without
-- the name's own definition site.
callerNamesOf :: RefIndex -> Text -> Text -> [Text]
callerNamesOf idx modName name =
  nub
    [ trModule r <> "." <> trFunction r
    | r <- Map.findWithDefault [] (modName, name) idx
    , not (trModule r == modName && trFunction r == name)
    ]

-- | Attach the callers of each signature, foreign import and pattern.
-- Always Just, so the renderer prints a line for every name, the same
-- way coverage prints "no test references".
attachCallers :: RefIndex -> ChaseFile -> ChaseFile
attachCallers idx cf = cf
  { chaseCallers = Just $
       [ (sigName s, look (sigName s)) | s <- chaseSignatures     cf ]
    <> [ (sigName s, look (sigName s)) | s <- chaseForeignImports cf ]
    <> [ (patName p, look (patName p)) | p <- chasePatterns       cf ]
  }
  where
    look k =
      [ r
      | r <- Map.findWithDefault [] (chaseModuleName cf, k) idx
      , not (trModule r == chaseModuleName cf && trFunction r == k)
      ]

-- | A hand-written topology against the derived edges of the type its
-- vertices name.
--
-- A topology is matched to the derived type whose constructor set
-- contains every vertex. With no match there is nothing to check and one
-- line says so. With a match: a declared edge nothing witnesses, a
-- witnessed edge the topology leaves out, and a vertex outside the
-- terminal list with no outbound declared edge are each reported.
checkTopologyDrift :: [DerivedTopology] -> ChaseFile -> [Text]
checkTopologyDrift derived ChaseFile{..} = concatMap one chaseTopologies
  where
    one Topology{..} =
      case [ dt | dt <- derived, all (`elem` dtConstructors dt) topVertices, not (null topVertices) ] of
        [] ->
          [ "topology " <> topName
              <> ": its vertices name no constructor set in the parsed sources;"
              <> " no derived edges to check against" ]
        (dt : _) ->
          let declared = [ (f, t) | (f, tos) <- topTransitions, t <- tos ]
              witnessed (f, t) =
                [ deWitness e
                | e <- dtEdges dt
                , deTo e == t
                , deFrom e == f || deFrom e == "*"
                ]
              unwitnessed =
                [ "topology " <> topName <> ": edge " <> f <> " -> " <> t
                    <> " has no witness: no clause matches " <> f
                    <> " and builds " <> t <> " (derived from " <> dtModule dt <> "." <> dtType dt <> ")"
                | (f, t) <- declared
                , null (witnessed (f, t))
                ]
              undeclared =
                [ "topology " <> topName <> ": " <> deWitness e
                    <> " builds " <> deTo e <> " from " <> deFrom e
                    <> ", an edge the topology does not declare"
                | e <- nub (dtEdges dt)
                , deFrom e /= "*"
                , deFrom e /= deTo e
                , deFrom e `elem` topVertices
                , deTo e `elem` topVertices
                , (deFrom e, deTo e) `notElem` declared
                ]
              noExit =
                [ "topology " <> topName <> ": state " <> v
                    <> " is not listed as terminal and has no outbound edge"
                | not (null topTerminal)
                , v <- topVertices
                , v `notElem` topTerminal
                , null [ () | (f, tos) <- topTransitions, f == v, not (null tos) ]
                ]
          in unwitnessed <> undeclared <> noExit

-- | A hand-written consumes entry whose leading token is a
-- Module.function that the caller index does not list as a caller of
-- the invariant's function. Entries that start with anything else (a
-- column name, "the", a parenthetical) are not checked, since consumes
-- is wider than the call graph.
checkConsumesDrift :: RefIndex -> ChaseFile -> [Text]
checkConsumesDrift idx ChaseFile{..} =
  [ "invariant " <> invFunction i <> " consumes " <> tok
      <> ", which is not among its callers in the scanned roots"
  | i <- chaseInvariants
  , c <- invConsumes i
  , (tok : _) <- [T.words c]
  , looksQualified tok
  , tok `notElem` callerNamesOf idx chaseModuleName (invFunction i)
  ]
  where
    looksQualified t =
      case T.splitOn "." t of
        parts@(_ : _ : _) ->
          all (startsWith isUpper) (init parts) && startsWith isLower (last parts)
        _ -> False
    startsWith p s = case T.uncons s of
      Just (ch, _) -> p ch
      Nothing      -> False

-- | Constant candidates the annotation file does not already name, as
-- lines for the %const_candidate section.
checkConstCandidates :: ChaseFile -> [(Text, Text)]
checkConstCandidates ChaseFile{..} =
  case chaseDerived of
    Nothing -> []
    Just d  ->
      [ (n, v)
      | (n, v) <- derConstCands d
      , n `notElem` map constName chaseConstants
      ]