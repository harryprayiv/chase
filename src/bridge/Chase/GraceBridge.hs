{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Glue layer between chase and grace.
--
-- The grace file at `graceTemplate` is a typed function decoded as a
-- Haskell function via Grace's FromGrace (a -> IO b) instance. The grace
-- file is the prompt template, externalized and version controlled.
-- This module never invokes the OpenAI client directly; grace does.
--
-- ============================================================================
-- THE GENERATOR GETS THE SOURCE AS WELL AS THE SKELETON
-- ============================================================================
--
-- GenArgs carries `source` since 2026-09-26. Before that the generator
-- saw only the bundle, and a bundle has no function bodies by design.
-- Asking a model for behavioural facts from signatures alone is asking
-- for something the input does not contain, and a local 7B answered the
-- only way it could: by restating the type, nine times out of nine, twice
-- through a retry loop that told it not to.
--
-- The skeleton stays because it carries what the source does not: every
-- signature in one place, the imports collapsed, the invariants already
-- recorded for neighbouring functions, and the module's decisions. The
-- source carries what the skeleton drops: what each function actually
-- does.
module Chase.GraceBridge
  ( GenArgs (..)
  , GenAnnotations (..)
  , GenInvariant (..)
  , GenDecision (..)
  , GenOpenIssue (..)
  , loadGenerator
  , generateWithDriftFeedback
  , toModuleAnnotations
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON, ToJSON)
import Data.Text              (Text)
import GHC.Generics           (Generic)
import Grace.Decode           (FromGrace, Key (..), ToGraceType)
import Grace.Encode           (ToGrace)
import Grace.Input            (Input (..), Mode (..))
import Grace.Interpret        (load)

import qualified Chase.Pipeline as Pipeline
import qualified Chase.Types    as CT

-- | Inputs the grace prompt template expects, as a record.
data GenArgs = GenArgs
  { key            :: Key
  , bundle         :: Text
  , source         :: Text
  , modName        :: Text
  , driftWarnings  :: [Text]
  } deriving stock    (Generic, Show)
    deriving anyclass (ToGrace, ToGraceType)

data GenAnnotations = GenAnnotations
  { invariants :: [GenInvariant]
  , decisions  :: [GenDecision]
  , openIssues :: [GenOpenIssue]
  } deriving stock    (Generic, Show)
    deriving anyclass (FromGrace, ToGraceType, FromJSON, ToJSON)

-- | One invariant attached to a signature. `body` is the list of
-- behavioural facts that render under the signature in chase output
-- (the lines prefixed with `!`). Named `body` rather than `lines` so it
-- does not shadow `Prelude.lines` under `RecordWildCards`.
data GenInvariant = GenInvariant
  { function :: Text
  , body     :: [Text]
  , consumes :: [Text]
  } deriving stock    (Generic, Show)
    deriving anyclass (FromGrace, ToGraceType, FromJSON, ToJSON)

data GenDecision = GenDecision
  { name    :: Text
  , what    :: Text
  , why     :: Text
  , affects :: [Text]
  } deriving stock    (Generic, Show)
    deriving anyclass (FromGrace, ToGraceType, FromJSON, ToJSON)

data GenOpenIssue = GenOpenIssue
  { name     :: Text
  , what     :: Text
  , why      :: Text
  , blocking :: [Text]
  , affects  :: [Text]
  } deriving stock    (Generic, Show)
    deriving anyclass (FromGrace, ToGraceType, FromJSON, ToJSON)

-- | Decode the grace template as a typed Haskell function.
loadGenerator
  :: MonadIO m
  => FilePath
  -> m (GenArgs -> IO GenAnnotations)
loadGenerator graceTemplate =
  load (Path graceTemplate AsCode)

-- | Run the generator, run chase's drift checker, feed the warnings back
-- and try again, up to maxRetries times. Returns the BEST attempt and
-- its warnings.
--
-- Best, not last. Measured 2026-09-26 against a local 7B on
-- Pelotero.DB.Pool: attempt one produced ten invariants (eight of them
-- restatements of the type, which the checker warned about); attempt two,
-- given those warnings, deleted the invariants entirely and returned two
-- plus eight fabricated open issues. Keeping the last attempt threw away
-- the better one. The score is (invariants that pass every check,
-- warnings), and more good invariants wins.
generateWithDriftFeedback
  :: MonadIO m
  => (GenArgs -> IO GenAnnotations)
  -> Int             -- ^ maxRetries
  -> CT.ChaseFile    -- ^ the parsed file these annotations attach to
  -> Key             -- ^ API key, opaque to this module
  -> Text            -- ^ bundle text (chase output for this module)
  -> Text            -- ^ the module's source, which the bundle omits
  -> m (GenAnnotations, [Text])
generateWithDriftFeedback gen maxRetries chaseFile k bundle src =
  liftIO (loop 0 [] Nothing)
  where
    modName = CT.chaseModuleName chaseFile

    attemptOf result =
      let modAnn = toModuleAnnotations modName result
          merged = mergeForCheck chaseFile modAnn
      in (result, Pipeline.checkAnnotationDrift merged, Pipeline.annotationScore merged)

    better (_, _, (g1, w1)) (_, _, (g2, w2)) =
      g1 > g2 || (g1 == g2 && w1 < w2)

    keepBest new Nothing = new
    keepBest new (Just old) = if better new old then new else old

    finish (result, warnings, _) = pure (result, warnings)

    loop attempt warnings best = do
      result <- gen (GenArgs k bundle src modName warnings)
      let this = attemptOf result
          (_, fresh, _) = this
          bestSoFar = keepBest this best
      if null fresh
        then finish this
        else
          if attempt >= maxRetries
            then finish bestSoFar
            else loop (attempt + 1) fresh (Just bestSoFar)

-- | Convert the grace-shaped result into chase's existing types.
toModuleAnnotations :: Text -> GenAnnotations -> CT.ModuleAnnotations
toModuleAnnotations modNm GenAnnotations{..} = CT.ModuleAnnotations
  { CT.annModName    = modNm
  , CT.annConstants  = []
  , CT.annInvariants = map invToInv invariants
  , CT.annDecisions  = map decToDec decisions
  , CT.annOpenIssues = map oiToOi   openIssues
  , CT.annTopologies = []
  }
  where
    invToInv GenInvariant{..} = CT.Invariant
      { CT.invFunction = function
      , CT.invLines    = body
      , CT.invConsumes = consumes
      , CT.invBodyHint = Nothing
      }

    decToDec GenDecision{..} = CT.Decision
      { CT.decName    = name
      , CT.decWhat    = what
      , CT.decWhy     = why
      , CT.decAffects = affects
      }

    oiToOi GenOpenIssue{..} = CT.OpenIssue
      { CT.oiName     = name
      , CT.oiWhat     = what
      , CT.oiWhy      = why
      , CT.oiBlocking = blocking
      , CT.oiAffects  = affects
      }

-- | Attach the candidate annotations to the parsed file in memory so
-- drift checking can run against the actual structure.
mergeForCheck :: CT.ChaseFile -> CT.ModuleAnnotations -> CT.ChaseFile
mergeForCheck cf CT.ModuleAnnotations{..} = cf
  { CT.chaseInvariants = annInvariants
  , CT.chaseDecisions  = annDecisions
  , CT.chaseOpenIssues = annOpenIssues
  , CT.chaseConstants  = annConstants
  , CT.chaseTopologies = annTopologies
  }