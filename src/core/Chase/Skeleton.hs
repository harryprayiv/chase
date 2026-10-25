{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A skeleton annotations entry for a module, from what the derive pass
-- already knows. Everything mechanical is filled in; the prose (intent,
-- notes, decisions, open issues, open questions) and the terminal states
-- of each topology are left for the author.
--
-- This is the deterministic half of Allium's distill: the structure of
-- every rule (what it requires, how it fails, what it touches, who calls
-- it) comes from the code, and only the meaning is written by hand or
-- by a model.
module Chase.Skeleton
  ( skeletonEntry
  ) where

import qualified Data.Aeson        as A
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import           Data.List         (nub, sort)
import           Data.Text         (Text)

import           Chase.Types

-- | The module's skeleton entry, from its merged skeleton (derived facts,
-- HIE facts and callers attached) and the run's derived topologies.
skeletonEntry :: [DerivedTopology] -> ChaseFile -> A.Object
skeletonEntry tops cf@ChaseFile{..} = KM.fromList
  [ ("invariants",    A.Object (KM.fromList [ (Key.fromText (sigName s), invariantFor cf (sigName s)) | s <- chaseSignatures <> chaseForeignImports ]))
  , ("constants",     A.toJSON [ A.object [ "name" A..= n, "value" A..= v, "notes" A..= ([] :: [Text]) ] | (n, v) <- maybe [] derConstCands chaseDerived ])
  , ("topologies",    A.toJSON [ topologyFor t | t <- tops, dtModule t == chaseModuleName ])
  , ("decisions",     A.toJSON ([] :: [A.Value]))
  , ("openIssues",    A.toJSON ([] :: [A.Value]))
  , ("openQuestions", A.toJSON ([] :: [Text]))
  ]

invariantFor :: ChaseFile -> Text -> A.Value
invariantFor ChaseFile{..} name = A.object
  [ "intent"   A..= ("" :: Text)
  , "requires" A..= lookupAll name (maybe [] derClauses chaseDerived)
  , "ensures"  A..= ([] :: [Text])
  , "fails"    A..= lookupAll name (maybe [] derFails chaseDerived)
  , "effects"  A..= lookupAll name (maybe [] hfUses chaseHie)
  , "consumes" A..= [ trModule r <> "." <> trFunction r | r <- concat (lookupAll' name chaseCallers) ]
  , "notes"    A..= ([] :: [Text])
  ]
  where
    lookupAll k kvs = concat [ v | (k', v) <- kvs, k' == k ]
    lookupAll' k = maybe [] (\cs -> [ v | (k', v) <- cs, k' == k ])

-- | A topology from the derived edges of one type: every constructor is
-- a vertex, every edge with a known source is a transition, and the
-- terminal list is empty for the author to fill.
topologyFor :: DerivedTopology -> A.Value
topologyFor DerivedTopology{..} = A.object
  [ "name"        A..= dtType
  , "vertices"    A..= dtConstructors
  , "transitions" A..= A.Object (KM.fromList
      [ (Key.fromText from, A.toJSON (sort (nub [ deTo e | e <- dtEdges, deFrom e == from ])))
      | from <- dtConstructors
      , any ((== from) . deFrom) dtEdges
      ])
  , "terminal"    A..= ([] :: [Text])
  ]
