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

-- | Run the generator, then run chase's drift checker, then loop on
-- drift warnings up to maxRetries times. Returns the last attempt.
generateWithDriftFeedback
  :: MonadIO m
  => (GenArgs -> IO GenAnnotations)
  -> Int             -- ^ maxRetries
  -> CT.ChaseFile    -- ^ the parsed file these annotations attach to
  -> Key             -- ^ OpenAI key
  -> Text            -- ^ bundle text (chase output for this module)
  -> m (GenAnnotations, [Text])
generateWithDriftFeedback gen maxRetries chaseFile k bundle =
  liftIO (loop 0 [])
  where
    modName = CT.chaseModuleName chaseFile

    loop attempt warnings
      | attempt >= maxRetries = do
          result <- gen (GenArgs k bundle modName warnings)
          let modAnn = toModuleAnnotations modName result
          let fresh  = Pipeline.checkAnnotationDrift
                         (mergeForCheck chaseFile modAnn)
          pure (result, fresh)
      | otherwise = do
          result <- gen (GenArgs k bundle modName warnings)
          let modAnn = toModuleAnnotations modName result
          let fresh  = Pipeline.checkAnnotationDrift
                         (mergeForCheck chaseFile modAnn)
          if null fresh
            then pure (result, [])
            else loop (attempt + 1) fresh

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