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
-- WHAT THE GENERATOR IS GIVEN, AND WHY
-- ============================================================================
--
-- Three inputs, and each one exists because its absence produced a
-- measured failure against a local 7B on Pelotero.DB.Pool, 2026-09-26.
--
-- bundle: the rendered skeleton. Every signature in one place, the
-- imports, the data declarations, the invariants already recorded for
-- neighbouring functions.
--
-- source: the module's text, WITH the function bodies the skeleton drops
-- by design. Without it the model was asked for behavioural facts from a
-- prompt containing no behaviour and restated the type nine times, through
-- two retries that told it not to.
--
-- callers: who calls each function, derived by scanning the project's
-- declared source directories. Asked to produce this itself, the model
-- returned module names from the %uses lines.
--
-- ============================================================================
-- EVERY NOTE IS CHECKED AGAINST ITS OWN FUNCTION
-- ============================================================================
--
-- With the source in the prompt, a new failure appeared: a note naming
-- something from the right FILE but the wrong FUNCTION. For
-- `release = Pool.release`, one line, the model wrote "uses 'dbPoolSize'
-- from 'DBConfig'". dbPoolSize is in the module, so a module-wide name
-- check passes it, and the claim is false.
--
-- So each note's identifiers are looked up in that function's own
-- definition (its signature plus its body, sliced from the source by
-- column). A note naming anything absent from there is removed from the
-- output and reported, so a retry can replace it. A note with no
-- identifier at all is kept: it cannot be checked this way, and the
-- restatement check in Chase.Pipeline is what judges those.
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
import Data.Char              (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.Text              (Text)
import qualified Data.Text    as T
import GHC.Generics           (Generic)
import Grace.Decode           (FromGrace, Key (..), ToGraceType)
import Grace.Encode           (ToGrace)
import Grace.Input            (Input (..), Mode (..))
import Grace.Interpret        (load)

import qualified Chase.Callers  as Callers
import qualified Chase.Pipeline as Pipeline
import qualified Chase.Types    as CT

-- | Inputs the grace prompt template expects, as a record.
data GenArgs = GenArgs
  { key            :: Key
  , bundle         :: Text
  , source         :: Text
  , callers        :: Text
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
--
-- No consumes field: that is derived from the call graph, not asked for.
data GenInvariant = GenInvariant
  { function :: Text
  , body     :: [Text]
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

-- | Run the generator, check the result, feed the warnings back and try
-- again, up to maxRetries times. Returns the BEST attempt, with any note
-- that names something absent from its own function removed.
--
-- Best, not last. Measured 2026-09-26: attempt one produced ten
-- invariants (eight of them restatements, which the checker warned
-- about); attempt two, given those warnings, deleted the invariants
-- entirely and returned two plus eight fabricated open issues. Keeping
-- the last attempt threw away the better one. The score is (invariants
-- that pass every check, warnings), and more good invariants wins.
generateWithDriftFeedback
  :: MonadIO m
  => (GenArgs -> IO GenAnnotations)
  -> Int                 -- ^ maxRetries
  -> Callers.CallerIndex -- ^ project-wide call graph
  -> CT.ChaseFile        -- ^ the parsed file these annotations attach to
  -> Key                 -- ^ API key, opaque to this module
  -> Text                -- ^ bundle text (chase output for this module)
  -> Text                -- ^ the module's source, which the bundle omits
  -> Text                -- ^ rendered caller lines
  -> m (GenAnnotations, [Text])
generateWithDriftFeedback gen maxRetries callerIdx chaseFile k bundle src callerText =
  liftIO (loop 0 [] Nothing)
  where
    modNm = CT.chaseModuleName chaseFile

    -- An attempt: the result with unsupported notes removed, all warnings
    -- (the pipeline's plus the scoped-name ones), and the score of what
    -- is left.
    attemptOf raw =
      let (cleaned, scoped) = dropUnsupportedNotes chaseFile src raw
          modAnn = toModuleAnnotations callerIdx modNm cleaned
          merged = mergeForCheck chaseFile modAnn
          (good, pipelineWarnings) = Pipeline.annotationScore merged
          warnings = Pipeline.checkAnnotationDrift merged <> scoped
      in (cleaned, warnings, (good, pipelineWarnings + length scoped))

    better (_, _, (g1, w1)) (_, _, (g2, w2)) =
      g1 > g2 || (g1 == g2 && w1 < w2)

    keepBest new Nothing = new
    keepBest new (Just old) = if better new old then new else old

    finish (result, warnings, _) = pure (result, warnings)

    loop attempt warnings best = do
      result <- gen (GenArgs k bundle src callerText modNm warnings)
      let this = attemptOf result
          (_, fresh, _) = this
          bestSoFar = keepBest this best
      if null fresh
        then finish this
        else
          if attempt >= maxRetries
            then finish bestSoFar
            else loop (attempt + 1) fresh (Just bestSoFar)

-- | Remove every invariant line that names something absent from its own
-- function's definition, and say which, so a retry can replace it.
--
-- An invariant left with no lines is dropped entirely: an empty invariant
-- is exactly the vacuous annotation the template tells the model not to
-- write.
dropUnsupportedNotes :: CT.ChaseFile -> Text -> GenAnnotations -> (GenAnnotations, [Text])
dropUnsupportedNotes chaseFile src ga@GenAnnotations{..} =
  let checked = map checkOne invariants
      kept = [ inv | (Just inv, _) <- checked ]
      warns = concat [ ws | (_, ws) <- checked ]
  in (ga { invariants = kept }, warns)
  where
    sigOf fn =
      case [ CT.sigVerbatim s | s <- CT.chaseSignatures chaseFile, CT.sigName s == fn ] of
        (s : _) -> s
        [] -> ""

    checkOne inv@GenInvariant{..} =
      let scope = T.toLower (sigOf function <> "\n" <> definitionOf function src)
          judge line =
            case [ w | w <- noteIdentifiers line, not (T.toLower w `T.isInfixOf` scope) ] of
              [] -> Right line
              missing -> Left (line, missing)
          results = map judge body
          keptLines = [ l | Right l <- results ]
          dropped = [ (l, m) | Left (l, m) <- results ]
          warnsFor =
            [ "invariant for " <> function <> " says \"" <> T.take 80 l <> "\", but "
                <> T.intercalate ", " (take 3 m)
                <> " does not appear in " <> function
                <> "'s own definition. Describe only what " <> function
                <> "'s body does; facts about its neighbours belong to them."
            | (l, m) <- dropped
            ]
      in if null keptLines
           then (Nothing, warnsFor)
           else (Just inv { body = keptLines }, warnsFor)

-- | One declaration's text: from the first line that starts with its name
-- in column zero (normally the signature) through every following line
-- that is indented or starts with the same name again, so a
-- multi-equation definition and its where-clause stay together.
--
-- Not a parser. It is exact for the layout chase's own code and
-- pelotero-engine use, and it errs towards including too much rather than
-- too little, which makes the check above lenient rather than harsh.
definitionOf :: Text -> Text -> Text
definitionOf fn src =
  let ls = T.lines src
      startsWithName l = (fn <> " ") `T.isPrefixOf` l || (fn <> "\t") `T.isPrefixOf` l
      isTop l = not (T.null l) && not (isSpace (T.head l))
  in case break startsWithName ls of
       (_, []) -> ""
       (_, first : rest) ->
         T.unlines (first : takeWhile (\l -> not (isTop l) || startsWithName l) rest)

-- | Identifiers a note names: code-shaped tokens (a dot, an inner or
-- leading capital, a digit, an underscore) and anything the model put in
-- single quotes, which is how it writes names in practice
-- ('envText', 'Pool.release'). Plain lower-case English is ignored.
--
-- A narrower copy of Chase.Pipeline's identifier test, kept local rather
-- than exported because this check is specific to generation.
noteIdentifiers :: Text -> [Text]
noteIdentifiers t =
  [ w
  | raw <- T.split (\c -> not (isAlphaNum c || c `elem` ("._'" :: String))) t
  , let w = T.dropAround (`elem` (".,;:!?'" :: String)) raw
  , T.length w >= 2
  , codeLike w || quoted w
  ]
  where
    quotedNames =
      [ T.takeWhile (/= '\'') rest
      | chunk <- drop 1 (T.splitOn "'" t)
      , let rest = chunk
      , not (T.null rest)
      ]
    quoted w = w `elem` quotedNames && T.all (\c -> isAlphaNum c || c `elem` ("._" :: String)) w
    codeLike w
      | T.length w == 2 = T.any isDigit w && T.any isAlpha w
    codeLike w =
      T.any (== '.') (T.drop 1 (T.init w))
        || T.any isUpper (T.drop 1 w)
        || (T.any isUpper (T.take 1 w) && T.any isAlpha w)
        || T.any isDigit w
        || T.any (== '_') w

-- | Convert the grace-shaped result into chase's existing types, filling
-- each invariant's consumes from the call graph rather than from the
-- model.
toModuleAnnotations
  :: Callers.CallerIndex -> Text -> GenAnnotations -> CT.ModuleAnnotations
toModuleAnnotations callerIdx modNm GenAnnotations{..} = CT.ModuleAnnotations
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
      , CT.invConsumes = Callers.callerNames callerIdx modNm function
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