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
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Options.Applicative
import           System.Directory     (doesDirectoryExist, doesFileExist)
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure)
import           System.FilePath      ((</>))
import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
import           System.IO            (hPutStrLn, hSetEncoding, stderr, stdin, stdout)

import           Chase.Annotations.Json (writeModuleAnnotations)
import qualified Chase.Callers        as Callers
import           Chase.Derive.Hie     (hieCallerIndex)
import qualified Chase.GraceBridge    as GB
import           Chase.Hie            (readHieDir)
import           Chase.Inference      (defaultAnnotationsTarget, findProjectRoot, inferenceDirName)
import qualified Chase.Parse          as Parse
import qualified Chase.Render         as Render
import qualified Chase.Types          as CT
import           Grace.Decode         (Key (..))
import           Paths_chase          (getDataFileName)

data Opts = Opts
  { optSource     :: FilePath
  , optKeyFile    :: Maybe FilePath
  , optTemplate   :: Maybe FilePath
  , optOutput     :: Maybe FilePath
  , optMaxRetries :: Int
  , optRoots      :: [FilePath]
  , optHie        :: HieMode
  , optVerbose    :: Bool
  }

data HieMode = HieAuto | HieDir FilePath | HieOff

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
  <*> optional
        ( strOption
            ( long "template"
           <> metavar "FILE"
           <> help "Grace prompt template. Default: the genAnnotations.ffg installed with chase."
            )
        )
  <*> optional
        ( strOption
            ( long "output"
           <> short 'o'
           <> metavar "FILE"
           <> help "Output annotations JSON path. Default: the annotations file in the .inference directory of the project SOURCE is in, or chase-annotations.json there when it has none yet."
            )
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
           <> help "Directories to scan for callers when no HIE files are read. Repeatable. Defaults to the hs-source-dirs of the library and executable stanzas in the nearest .cabal file above SOURCE."
            )
        )
  <*> hieOpt
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "List every file the caller scan could not parse"
        )
  where
    hieOpt =
          ( HieDir <$> strOption
              ( long "hie"
             <> metavar "DIR"
             <> help "Directory holding the project's .hie files. Callers are then resolved by the compiler and --roots is not used. Default: .inference/hie in the project SOURCE is in, when it exists."
              )
          )
      <|> flag' HieOff
              ( long "no-hie"
             <> help "Do not read .hie files even if .inference/hie exists."
              )
      <|> pure HieAuto

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

-- | The HIE directory: the one given, which must exist, or .inference/hie
-- in the project the source belongs to, when that exists, or none.
resolveHie :: FilePath -> HieMode -> IO (Maybe FilePath)
resolveHie _ HieOff     = pure Nothing
resolveHie _ (HieDir d) = do
  ok <- doesDirectoryExist d
  if ok then pure (Just d) else do
    hPutStrLn stderr ("hie directory not found: " <> d)
    exitFailure
resolveHie source HieAuto =
  findProjectRoot source >>= \case
    Nothing   -> pure Nothing
    Just root -> do
      let d = root </> inferenceDirName </> "hie"
      ok <- doesDirectoryExist d
      pure (if ok then Just d else Nothing)

-- | Every file chase reads or writes is UTF-8, whatever the locale. GHC
-- opens files and the standard handles in the locale's encoding, and under
-- the POSIX locale that is ASCII: on 2026-10-24 ten of cheeblr's frontend
-- modules failed with "cannot decode byte sequence" because they hold a
-- character outside ASCII, and their annotations were reported as matching
-- no source file. A remote command run over ssh can get that locale.
utf8Everywhere :: IO ()
utf8Everywhere = do
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]

main :: IO ()
main = do
  utf8Everywhere
  Opts{..} <- execParser (info (helper <*> opts) fullDesc)

  key <- resolveKey optKeyFile

  output <- case optOutput of
    Just o  -> pure o
    Nothing ->
      defaultAnnotationsTarget optSource >>= \case
        Just o  -> pure o
        Nothing -> do
          hPutStrLn stderr
            ( "no output given and " <> optSource
                <> " is in no project with a .inference directory; pass -o FILE or create .inference at the project root" )
          exitFailure

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

      -- Callers come from the project's HIE files when there are any,
      -- resolved by the compiler. Otherwise the call graph is read from
      -- the project's own declared source directories, or from explicit
      -- --roots as given.
      hieDir <- resolveHie optSource optHie
      hie <- case hieDir of
        Nothing -> pure []
        Just d  -> do
          (mods, warnings) <- readHieDir d
          mapM_ (\w -> TIO.hPutStrLn stderr ("hie: " <> w)) warnings
          putStrLn ("hie: " <> show (length mods) <> " modules from " <> d)
          pure mods

      callerIdx <-
        if not (null hie)
          then pure (hieCallerIndex hie)
          else do
            plan <- case optRoots of
              [] -> Callers.planScan optSource
              rs -> pure Callers.ScanPlan { Callers.spRoots = rs, Callers.spSource = "--roots as given" }
            Callers.buildCallerIndex optVerbose plan

      let names =
            map CT.sigName (CT.chaseSignatures chaseFile)
              <> map CT.sigName (CT.chaseForeignImports chaseFile)
          callerText = Callers.renderCallers callerIdx modName names

      -- The template ships with chase as a data file, so it is found
      -- wherever chase-annotate runs. It used to default to the relative
      -- path grace/genAnnotations.ffg, which exists only in chase's own
      -- checkout.
      template <- maybe (getDataFileName "grace/genAnnotations.ffg") pure optTemplate
      gen <- GB.loadGenerator template

      (gen', drift) <- GB.generateWithDriftFeedback
                         gen optMaxRetries callerIdx chaseFile
                         (Key key) bundle src callerText

      let modAnn   = GB.toModuleAnnotations callerIdx modName gen'

      -- Only this module's generated fields are replaced. The rest of the
      -- file, and this module's constants and topologies, are kept.
      writeModuleAnnotations output modName (moduleFields modAnn) >>= \case
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right () -> pure ()

      mapM_ (\w -> TIO.hPutStrLn stderr ("drift: " <> w)) drift
      putStrLn ("wrote " <> T.unpack modName <> " into " <> output)

-- | The fields chase-annotate generates for one module.
moduleFields :: CT.ModuleAnnotations -> A.Object
moduleFields CT.ModuleAnnotations{..} = KM.fromList
  [ ("invariants", invariantsToJSON annInvariants)
  , ("decisions",  A.toJSON (map decisionToJSON annDecisions))
  , ("openIssues", A.toJSON (map openIssueToJSON annOpenIssues))
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