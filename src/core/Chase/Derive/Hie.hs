{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Facts from the HIE model: the resolved caller index, effectful
-- external callees per function, unreferenced bindings, and constructors
-- nothing builds or nothing matches.
--
-- "External" means defined in a package unit that no HIE file of the
-- run belongs to. A project of several packages, each with HIE files in
-- the directory, counts as one project.
module Chase.Derive.Hie
  ( hieCallerIndex
  , hieFactsFor
  , constructorUsage
  ) where

import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Chase.Derive    (RefIndex)
import           Chase.Hie
import           Chase.Types

-- | (defining module, name) to the bindings that reference it, for value
-- references. Constructor and type references are left out; the usage
-- pass covers constructors.
hieCallerIndex :: [HieModule] -> RefIndex
hieCallerIndex mods =
  Map.map (sortOn trSrcLine) $ Map.fromListWith (<>)
    [ ((hrModule r, hrName r), [TestRef (hmModule m) (hbName b) (hbLine b)])
    | m <- mods
    , b <- hmBindings m
    , r <- hbRefs b
    , hrKind r == RefValue
    ]

-- | The facts for one skeleton: external callees whose type at the use
-- site is effectful, per binding, and the bindings that are neither
-- exported nor referenced anywhere.
hieFactsFor :: [HieModule] -> RefIndex -> Text -> Maybe HieFacts
hieFactsFor mods idx modName =
  case [ m | m <- mods, hmModule m == modName ] of
    []      -> Nothing
    (m : _) ->
      let units = Set.fromList (map hmUnit mods)
          uses  =
            [ (hbName b, capped 12 (Set.toList (Set.fromList
                [ hrModule r <> "." <> hrName r
                | r <- hbRefs b
                , hrKind r == RefValue
                , hrEffectful r
                , not (Set.member (hrUnit r) units)
                ])))
            | b <- hmBindings m
            ]
          unreferenced =
            [ hbName b
            | b <- hmBindings m
            , hbName b `notElem` hmExports m
            , null (Map.findWithDefault [] (modName, hbName b) idx)
            ]
      in Just HieFacts
        { hfUses         = filter (not . null . snd) uses
        , hfUnreferenced = unreferenced
        }

-- | Constructors nothing builds, and constructors of multi-constructor
-- types nothing matches, each with the bindings on the other side.
constructorUsage :: [HieModule] -> [ConstructorUsage]
constructorUsage mods =
  [ ConstructorUsage (hmModule m) t c builders matchers
  | m <- mods
  , (t, cons) <- hmConstructors m
  , c <- cons
  , let builders = who RefBuild (hmModule m) c
        matchers = who RefMatch (hmModule m) c
  , null builders || (null matchers && length cons >= 2)
  ]
  where
    who kind modName c =
      Set.toList $ Set.fromList
        [ hmModule m <> "." <> hbName b
        | m <- mods
        , b <- hmBindings m
        , r <- hbRefs b
        , hrKind r == kind
        , hrModule r == modName
        , hrName r == c
        ]

capped :: Int -> [Text] -> [Text]
capped n xs
  | length xs <= n = xs
  | otherwise      = take n xs <> ["+" <> T.pack (show (length xs - n)) <> " more"]