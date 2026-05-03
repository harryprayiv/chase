{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Chase.Parse
  ( parseSourceFile
  , ParseFailure (..)
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as BS

-- ghc-lib-parser. Imports kept narrow so type errors point at the
-- exact module that needs adjusting.
import GHC.Data.FastString    (mkFastString)
import GHC.Data.StringBuffer  (stringToStringBuffer)
import GHC.Driver.Session     (defaultDynFlags)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Parser             (parseModule)
import GHC.Parser.Lexer       (ParseResult (..), initParserState, unP)
import GHC.Types.SrcLoc       (mkRealSrcLoc)

import Chase.Types  -- for ChaseFile, but we won't build one yet

------------------------------------------------------------------

data ParseFailure = ParseFailure
  { pfPath :: FilePath
  , pfMsg  :: Text
  }

-- | Step-1 stub. Returns an EMPTY ChaseFile on parse success and a
-- diagnostic on failure. AST extraction comes later, once we know
-- the parser plumbing compiles.
parseSourceFile :: FilePath -> IO (Either ParseFailure ChaseFile)
parseSourceFile path = do
  bytes <- BS.readFile path
  let sourceText = TE.decodeUtf8 bytes
      buf        = stringToStringBuffer (T.unpack sourceText)
      -- HACK: defaultDynFlags wants Settings + LlvmConfig args. We
      -- don't have those yet. This call WILL fail to typecheck and
      -- the error will tell us the real arity. That's deliberate
      -- for round 1.
      dflags     = defaultDynFlags undefined undefined
      pOpts      = initParserOpts dflags
      startLoc   = mkRealSrcLoc (mkFastString path) 1 1
      pstate0    = initParserState pOpts buf startLoc
  case unP parseModule pstate0 of
    POk _ _      -> pure $ Right (emptyChaseFile path)
    PFailed _    -> pure $ Left  $ ParseFailure path "parse failed"