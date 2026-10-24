{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Probes for suspected defects on the unison branch. Each test asserts the
-- behaviour the spec (chase.allium) requires; a failure is evidence for the
-- matching issue. U-numbers match the spec's comments.
module Main (main) where

import qualified Data.ByteString          as BS
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           System.Directory         ( createDirectoryIfMissing, createDirectoryLink
                                          , getTemporaryDirectory, removeFile, removePathForcibly )
import           System.Exit              (exitFailure, exitSuccess)
import           System.FilePath          (takeDirectory, (</>))

import Chase.Annotations.Json (loadAnnotationsIfExists)
import qualified Chase.Coverage as Coverage
import qualified Chase.Pipeline as Pipeline
import Chase.Sources (findSourceFiles)
import Chase.Types
import Chase.Unison.Annotate  (AnnotatedResult (..), renderAnnotated)
import Chase.Unison.Extract   (SkelEntry (..), SkelKind (..), UnisonSkeleton (..))

-- | One definition known by two names: "parse" (best) and "json.parse".
skeleton :: UnisonSkeleton
skeleton = UnisonSkeleton
  { uskProject = "probe"
  , uskBranch  = "main"
  , uskRoot    = ""
  , uskEntries =
      [ SkelEntry "#abcdef0123456789" ["parse", "json.parse"] (SkelTerm "Text -> Optional Json") ]
  }

-- | U1: an invariant written against an alias. The drift check accepts it,
-- because every name is in the name set, but only the best name's
-- invariant is rendered, so it vanishes without a warning.
aliasInvariant :: IO Bool
aliasInvariant = do
  let anns = (emptyAnnotations "probe")
        { annInvariants = [inv "json.parse" ["rejects trailing commas"]] }
      res = renderAnnotated skeleton anns
      shown = "rejects trailing commas" `T.isInfixOf` arText res
      warned = not (null (arDriftWarnings res))
  report "U1 an invariant on an alias is rendered or reported"
         (shown || warned)
         ("rendered: " <> show shown <> ", drift warnings: " <> show (arDriftWarnings res))

-- | U2: chase-unison loads its annotations with loadAnnotationsIfExists,
-- which turned a parse error into an empty map, so a typo in the file gave
-- a bundle with no annotations and no message. A missing file is still no
-- annotations, without an error.
malformedFile :: IO Bool
malformedFile = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "chase-probe-annotations.json"
  writeFile path "{ \"version\": 1, \"modules\": { \"probe\": { \"invariants\": { \"parse\": 7 } } } }"
  bad <- loadAnnotationsIfExists path
  removeFile path
  missing <- loadAnnotationsIfExists path
  let reported = either (const True) (const False) bad
      quietWhenMissing = either (const False) Map.null missing
  report "U2 a malformed annotations file is an error, a missing one is none"
         (reported && quietWhenMissing)
         ("malformed: " <> either id (show . Map.keys) bad <> "; missing: " <> either id (show . Map.keys) missing)

-- | A scratch directory holding the given files, removed afterwards.
withTree :: String -> [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTree name files act = do
  tmp <- getTemporaryDirectory
  let root = tmp </> name
  removePathForcibly root
  mapM_ (\(f, body) -> createDirectoryIfMissing True (takeDirectory (root </> f)) >> writeFile (root </> f) body) files
  r <- act root
  removePathForcibly root
  pure r

-- | U3: two runs over the same code must give the same bundle bytes, or
-- open-slop's skip-if-unchanged hash never matches.
reproducibleBundle :: IO Bool
reproducibleBundle =
  withTree "chase-probe-repro" [("src/A.hs", "module A where\n\nf :: Int -> Int\nf = id\n")] \root -> do
    let cfg out = Pipeline.defaultConfig
          { Pipeline.cfgSourceRoots = [root </> "src"]
          , Pipeline.cfgBundleFile  = Just (root </> out)
          }
    Pipeline.runChase (cfg "one.chase")
    Pipeline.runChase (cfg "two.chase")
    one <- BS.readFile (root </> "one.chase")
    two <- BS.readFile (root </> "two.chase")
    report "U3 the same sources give the same bundle bytes" (one == two) "the two bundles differ"

-- | U4: discovery must not follow a symbolic link (a loop recursed
-- without end) nor enter build output or dot-directories.
discovery :: IO Bool
discovery =
  withTree "chase-probe-walk"
    [ ("src/A.hs", "module A where\n")
    , ("src/dist-newstyle/Paths_x.hs", "module Paths_x where\n")
    , ("src/.hidden/B.hs", "module B where\n")
    ] \root -> do
      createDirectoryLink (root </> "src") (root </> "src" </> "loop")
      found <- findSourceFiles (root </> "src")
      report "U4 discovery skips links, build output and dot-directories"
             (found == [root </> "src" </> "A.hs"])
             ("found: " <> show found)

-- | U5: two modules define parse; the test imports only A and calls
-- parse. Only A.parse is tested.
coverageByModule :: IO Bool
coverageByModule =
  withTree "chase-probe-cov"
    [ ("test/Spec.hs", "module Spec where\n\nimport A (parse)\nimport qualified B as Other\n\nspec = parse 1\nother = Other.render 2\n")
    ] \root -> do
      parsed <- Coverage.parseTestFile (root </> "test" </> "Spec.hs")
      case parsed of
        Left pf -> report "U5 coverage is resolved through imports" False (show pf)
        Right bindings -> do
          let idx = Coverage.buildTestIndex bindings
              has k = Map.member k idx
          report "U5 coverage is resolved through imports"
                 (has ("A", "parse") && not (has ("B", "parse")) && has ("B", "render") && not (has ("A", "render")))
                 ("index keys: " <> show (Map.keys idx))

report :: String -> Bool -> String -> IO Bool
report name ok detail = do
  putStrLn ((if ok then "PASS " else "FAIL ") <> name <> (if ok then "" else "\n     " <> detail))
  pure ok

main :: IO ()
main = do
  results <- sequence [aliasInvariant, malformedFile, reproducibleBundle, discovery, coverageByModule]
  if and results then exitSuccess else exitFailure