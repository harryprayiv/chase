{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Drive the grace annotation generator over one source file.
--
-- The API key is NEVER a command-line argument. An argv value is visible
-- in ps output, in shell history, and in the journal line of any systemd
-- unit that runs this program; all three were observed while testing
-- against a local gateway. Grace models the key as an opaque 'Key' whose
-- Show and ToJSON instances hide it, and taking it from argv defeats that
-- before Grace ever sees it.
--
-- So the key comes from, in order: --key-file, then $OPENAI_API_KEY_FILE,
-- then $OPENAI_API_KEY. A file is preferred to an environment variable
-- because /proc/<pid>/environ is readable by the same user and a
-- systemd unit's environment shows up in `systemctl show`.
--
-- The endpoint is Grace's business: set OPENAI_BASE_URL to point at a
-- local gateway or any other OpenAI-compatible server. This program does
-- not know an address.
module Main (main) where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Options.Applicative
import           System.Directory     (doesFileExist)
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

import qualified Chase.Callers        as Callers
import qualified Chase.GraceBridge    as GB
import qualified Chase.Parse          as Parse
import qualified Chase.Render         as Render
import qualified Chase.Types          as CT
import           Grace.Decode         (Key (..))

data Opts = Opts
  { optSource     :: FilePath
  , optKeyFile    :: Maybe FilePath
  , optTemplate   :: FilePath
  , optOutput     :: FilePath
  , optMaxRetries :: Int
  , optRoots      :: [FilePath]
  , optVerbose    :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> argument str
        ( metavar "SOURCE"
       <> help "Single source file to annotate (.hs or .purs)"
        )
  <*> optional
        ( strOption
            ( long "key-file"
           <> metavar "FILE"
           <> help "File holding the API key. Defaults to $OPENAI_API_KEY_FILE, then $OPENAI_API_KEY. Never pass a key as an argument."
            )
        )
  <*> strOption
        ( long "template"
       <> metavar "FILE"
       <> value "grace/genAnnotations.ffg"
       <> showDefault
       <> help "Grace prompt template"
        )
  <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> value "chase-annotations.json"
       <> showDefault
       <> help "Output annotations JSON path"
        )
  <*> option auto
        ( long "max-retries"
       <> metavar "N"
       <> value 2
       <> showDefault
       <> help "Drift feedback retry budget"
        )
  <*> many
        ( strOption
            ( long "roots"
           <> metavar "DIR"
           <> help "Directories to scan for callers. Repeatable. Defaults to the hs-source-dirs of the library and executable stanzas in the nearest .cabal file above SOURCE."
            )
        )
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "List every file the caller scan could not parse"
        )

-- | The key, from a file if one is named, else from the environment.
-- Fails with a message naming every place it looked, because a missing
-- key is the most common first-run failure and a bare 401 from the far
-- end explains nothing.
resolveKey :: Maybe FilePath -> IO Text
resolveKey explicit = do
  envFile <- lookupEnv "OPENAI_API_KEY_FILE"
  envKey  <- lookupEnv "OPENAI_API_KEY"
  case (explicit, envFile, envKey) of
    (Just p,  _,       _)      -> fromFile p
    (Nothing, Just p,  _)      -> fromFile p
    (Nothing, Nothing, Just k)
      | not (null k) -> pure (T.strip (T.pack k))
    _ -> do
      hPutStrLn stderr
        "no API key: pass --key-file FILE, or set OPENAI_API_KEY_FILE, or set OPENAI_API_KEY"
      exitFailure
  where
    fromFile p = do
      ok <- doesFileExist p
      if not ok
        then do
          hPutStrLn stderr ("no such key file: " <> p)
          exitFailure
        else do
          t <- T.strip <$> TIO.readFile p
          if T.null t
            then do
              hPutStrLn stderr ("the key file is empty: " <> p)
              exitFailure
            else pure t

main :: IO ()
main = do
  Opts{..} <- execParser (info (helper <*> opts) fullDesc)

  key <- resolveKey optKeyFile

  result <- Parse.parseSourceFile optSource
  case result of
    Left CT.ParseFailure{..} -> do
      hPutStrLn stderr ("parse failed: " <> pfPath)
      TIO.hPutStrLn stderr ("  " <> pfMsg)
      exitFailure

    Right chaseFile -> do
      let bundle  = Render.renderChaseFile chaseFile
      let modName = CT.chaseModuleName chaseFile

      -- The bundle omits function bodies on purpose. The generator is
      -- asked for behavioural facts, which live in the bodies.
      src <- TIO.readFile optSource

      -- The call graph is read from the project's own declared source
      -- directories. Explicit --roots override that and are used as given.
      plan <- case optRoots of
        [] -> Callers.planScan optSource
        rs -> pure Callers.ScanPlan { Callers.spRoots = rs, Callers.spSource = "--roots as given" }

      callerIdx <- Callers.buildCallerIndex optVerbose plan

      let names =
            map CT.sigName (CT.chaseSignatures chaseFile)
              <> map CT.sigName (CT.chaseForeignImports chaseFile)
          callerText = Callers.renderCallers callerIdx modName names

      gen <- GB.loadGenerator optTemplate

      (gen', drift) <- GB.generateWithDriftFeedback
                         gen optMaxRetries callerIdx chaseFile
                         (Key key) bundle src callerText

      let modAnn   = GB.toModuleAnnotations callerIdx modName gen'
      let jsonOut  = annotationsToJSON modName modAnn

      LBS.writeFile optOutput (A.encode jsonOut)

      mapM_ (\w -> TIO.hPutStrLn stderr ("drift: " <> w)) drift
      putStrLn ("wrote " <> optOutput)

annotationsToJSON :: Text -> CT.ModuleAnnotations -> A.Value
annotationsToJSON modName CT.ModuleAnnotations{..} = A.object
  [ "version" A..= (1 :: Int)
  , "modules" A..= A.object
      [ Key.fromText modName A..= A.object
          [ "invariants" A..= invariantsToJSON annInvariants
          , "decisions"  A..= map decisionToJSON annDecisions
          , "openIssues" A..= map openIssueToJSON annOpenIssues
          ]
      ]
  ]

invariantsToJSON :: [CT.Invariant] -> A.Value
invariantsToJSON invs = A.Object (KM.fromList (map entry invs))
  where
    entry CT.Invariant{..} =
      ( Key.fromText invFunction
      , A.object
          [ "notes"    A..= invLines
          , "consumes" A..= invConsumes
          ]
      )

decisionToJSON :: CT.Decision -> A.Value
decisionToJSON CT.Decision{..} = A.object
  [ "name"    A..= decName
  , "what"    A..= decWhat
  , "why"     A..= decWhy
  , "affects" A..= decAffects
  ]

openIssueToJSON :: CT.OpenIssue -> A.Value
openIssueToJSON CT.OpenIssue{..} = A.object
  [ "name"     A..= oiName
  , "what"     A..= oiWhat
  , "why"      A..= oiWhy
  , "blocking" A..= oiBlocking
  , "affects"  A..= oiAffects
  ]