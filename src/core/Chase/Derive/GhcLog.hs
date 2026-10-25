{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The compiler's own findings, read from a build log.
--
-- GHC's pattern-match checker decides, with the real types in hand,
-- which constructors a function does not handle and which of its
-- alternatives can never be reached. That is the check an Allium
-- transition graph calls dead transitions and deadlocks, done by the
-- compiler the project already runs. GHC 9.6 writes these as text, one
-- block per warning:
--
-- > src/Foo.hs:12:3: warning: [GHC-62161] [-Wincomplete-patterns]
-- >     Pattern match(es) are non-exhaustive
-- >     In an equation for ‘step’: Patterns of type ‘S’ not matched: R
--
-- A block starts with a line whose location ends in ".hs" or ".purs"
-- followed by ": warning:"; the indented lines after it are its message.
-- Errors are skipped, since a build with errors wrote no HIE files and
-- no usable warnings either. Nix's own "warning:" lines have no source
-- location and are skipped too.
--
-- Warnings appear only for modules the compiler actually compiled, so
-- the log must come from a full compile; the README gives the command.
module Chase.Derive.GhcLog
  ( parseGhcLog
  , attachGhcWarnings
  , unattached
  ) where

import           Data.Char       (isDigit, isSpace)
import           Data.List       (isSuffixOf, sortOn)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.FilePath (splitDirectories)

import           Chase.Types

-- | Every warning in a log, deduplicated by file, line and flag (a module
-- in several components is compiled once per component and warned about
-- each time).
parseGhcLog :: Text -> [GhcWarning]
parseGhcLog logText =
  dedupe (go (T.lines logText))
  where
    go [] = []
    go (l : ls) =
      case header l of
        Nothing -> go ls
        Just (file, line, flag) ->
          let (body, rest) = span isContinuation ls
              message = clip (T.unwords (map T.strip body))
          in GhcWarning file line flag message Nothing : go rest

    isContinuation l = not (T.null l) && isSpace (T.head l)

    dedupe = go' Set.empty
      where
        go' _ [] = []
        go' seen (w : ws)
          | Set.member key seen = go' seen ws
          | otherwise           = w : go' (Set.insert key seen) ws
          where key = (gwFile w, gwLine w, gwFlag w)

-- | The file, line and -W flag of a warning header line, if it is one.
header :: Text -> Maybe (Text, Int, Text)
header l =
  case T.breakOn ": warning:" l of
    (loc, rest)
      | T.null rest -> Nothing
      | otherwise ->
          case T.splitOn ":" loc of
            (file : lineText : _)
              | isSource file
              , Just n <- lineNumber lineText ->
                  Just (T.strip file, n, flagOf rest)
            _ -> Nothing
  where
    isSource f = ".hs" `T.isSuffixOf` f || ".purs" `T.isSuffixOf` f
    lineNumber t =
      let digits = T.takeWhile isDigit (T.dropWhile (== '(') (T.strip t))
      in if T.null digits then Nothing else Just (read (T.unpack digits))
    flagOf rest =
      case T.breakOn "[-W" rest of
        (_, r) | T.null r -> "warning"
               | otherwise -> T.takeWhile (/= ']') (T.drop 1 r)

-- | The warnings of a file, each attached to the nearest signature at or
-- above its line. The log's paths are relative to the package directory
-- and the skeleton's to the run's roots, so the match is by path suffix
-- in either direction.
attachGhcWarnings :: [GhcWarning] -> ChaseFile -> ChaseFile
attachGhcWarnings ws cf =
  cf { chaseGhcWarnings = sortOn gwLine [ attach w | w <- ws, sameFile (gwFile w) ] }
  where
    mine = splitDirectories (chaseSourcePath cf)
    sameFile f =
      let theirs = splitDirectories (T.unpack f)
      in theirs `isSuffixOf` mine || mine `isSuffixOf` theirs
    attach w =
      let above = [ (sigSrcLine s, sigName s) | s <- chaseSignatures cf, sigSrcLine s <= gwLine w ]
      in w { gwFunction = case above of
                            [] -> Nothing
                            _  -> Just (snd (maximum above)) }

-- | The warnings whose file matches none of the skeletons, so the run
-- can say they were read and went nowhere.
unattached :: [GhcWarning] -> [ChaseFile] -> [GhcWarning]
unattached ws cfs =
  [ w
  | w <- ws
  , let theirs = splitDirectories (T.unpack (gwFile w))
  , not (any (\cf -> let mine = splitDirectories (chaseSourcePath cf)
                     in theirs `isSuffixOf` mine || mine `isSuffixOf` theirs) cfs)
  ]

clip :: Text -> Text
clip t
  | T.length t <= 240 = t
  | otherwise         = T.take 237 t <> "..."
