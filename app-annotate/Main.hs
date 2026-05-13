{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Options.Applicative
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

import qualified Chase.GraceBridge    as GB
import qualified Chase.Parse          as Parse
import qualified Chase.Render         as Render
import qualified Chase.Types          as CT
import           Grace.Decode         (Key (..))

data Opts = Opts
  { optSource     :: FilePath
  , optKey        :: Text
  , optTemplate   :: FilePath
  , optOutput     :: FilePath
  , optMaxRetries :: Int
  }

opts :: Parser Opts
opts = Opts
  <$> argument str
        ( metavar "SOURCE"
       <> help "Single source file to annotate (.hs or .purs)"
        )
  <*> strOption
        ( long "key"
       <> metavar "KEY"
       <> help "OpenAI API key"
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

main :: IO ()
main = do
  Opts{..} <- execParser (info (helper <*> opts) fullDesc)

  result <- Parse.parseSourceFile optSource
  case result of
    Left CT.ParseFailure{..} -> do
      hPutStrLn stderr ("parse failed: " <> pfPath)
      TIO.hPutStrLn stderr ("  " <> pfMsg)
      exitFailure

    Right chaseFile -> do
      let bundle  = Render.renderChaseFile chaseFile
      let modName = CT.chaseModuleName chaseFile

      gen <- GB.loadGenerator optTemplate

      (gen', drift) <- GB.generateWithDriftFeedback
                         gen optMaxRetries chaseFile (Key optKey) bundle

      let modAnn   = GB.toModuleAnnotations modName gen'
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