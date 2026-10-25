{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Probes for suspected defects on the unison branch, and for the derive
-- pass. Each test asserts the behaviour the spec (chase.allium,
-- chase-derive.allium) requires; a failure is evidence for the matching
-- issue. U-numbers match chase.allium's comments; D-numbers are the
-- derive pass.
module Main (main) where

import qualified Data.ByteString          as BS
import           Data.List                (isInfixOf)
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           System.Directory         ( createDirectoryIfMissing, createDirectoryLink
                                          , getTemporaryDirectory, removeFile, removePathForcibly )
import           System.Exit              (exitFailure, exitSuccess)
import           System.FilePath          (takeDirectory, (</>))

import Chase.Annotations.Json (loadAnnotationsIfExists)
import qualified Chase.Coverage as Coverage
import Chase.Derive
import Chase.Parse (parseSourceFileFull)
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

-- derive pass ----------------------------------------------------------

-- | A skeleton for module M carrying one hand-written topology over the
-- vertices A, B, C.
withTopology :: [(T.Text, [T.Text])] -> [T.Text] -> ChaseFile
withTopology edges terminal =
  (emptyChaseFile LangHaskell "M.hs")
    { chaseModuleName = "M"
    , chaseTopologies = [Topology "Machine" ["A", "B", "C"] edges terminal]
    }

-- | Derived edges for a type S in module M with constructors A, B, C.
derivedS :: [(T.Text, T.Text)] -> [DerivedTopology]
derivedS es =
  [ DerivedTopology "S" "M" ["A", "B", "C"]
      [ DerivedEdge "M.S" f t "M.step" | (f, t) <- es ]
  ]

-- | D1: a declared edge with no clause witnessing it is reported, and a
-- witnessed one is not.
unwitnessedEdge :: IO Bool
unwitnessedEdge = do
  let ws = checkTopologyDrift (derivedS [("A", "B")]) (withTopology [("A", ["B"]), ("B", ["C"])] ["C"])
      texts = map T.unpack ws
      aboutBC = any ("edge B -> C has no witness" `isInfixOf`) texts
      aboutAB = any ("edge A -> B" `isInfixOf`) texts
  report "D1 a declared edge with no witness is reported, a witnessed one is not"
         (aboutBC && not aboutAB)
         ("warnings: " <> show ws)

-- | D2: a clause that produces an edge the topology does not declare is
-- reported, naming the witness.
undeclaredEdge :: IO Bool
undeclaredEdge = do
  let ws = checkTopologyDrift (derivedS [("A", "B"), ("B", "A")]) (withTopology [("A", ["B"]), ("B", ["C"])] ["C"])
      texts = map T.unpack ws
  report "D2 a witnessed edge the topology leaves out is reported"
         (any (\t -> "M.step builds A from B" `isInfixOf` t && "does not declare" `isInfixOf` t) texts)
         ("warnings: " <> show ws)

-- | D3: a vertex outside the terminal list with no outbound edge is
-- reported, and the check is skipped when no terminal list is given.
noExit :: IO Bool
noExit = do
  let withTerminal = checkTopologyDrift (derivedS [("A", "B")]) (withTopology [("A", ["B"])] ["C"])
      without      = checkTopologyDrift (derivedS [("A", "B")]) (withTopology [("A", ["B"])] [])
      flagged ws   = any (("state B is not listed as terminal" `isInfixOf`) . T.unpack) ws
      detail       = "with terminal: " <> show withTerminal <> " without: " <> show without
  report "D3 a non-terminal state with no exit is reported only when terminals are declared"
         (flagged withTerminal && not (flagged without))
         detail

-- | D4: edges come from real parses, across modules: S is declared in M
-- and transitioned in both M and N.
derivedEdges :: IO Bool
derivedEdges =
  withTree "chase-probe-edges"
    [ ("src/M.hs", "module M where\n\ndata S = P | Q | R\n\nstep :: S -> S\nstep P = Q\nstep Q = R\nstep R = R\n")
    , ("src/N.hs", "module N where\n\nimport M\n\ngo :: S -> S\ngo P = R\ngo s = s\n")
    ] \root -> do
      rm <- parseSourceFileFull (root </> "src" </> "M.hs")
      rn <- parseSourceFileFull (root </> "src" </> "N.hs")
      case (rm, rn) of
        (Right pm, Right pn) -> do
          let ps     = [pm, pn]
              conMap = buildConMap ps
              ds     = map (\p -> parsedDerive p conMap) ps
              tops   = mergeTopologies ps ds
              edges  = [ (deFrom e, deTo e, deWitness e) | t <- tops, e <- dtEdges t ]
              want   = [("P", "Q", "M.step"), ("Q", "R", "M.step"), ("R", "R", "M.step"), ("P", "R", "N.go")]
          report "D4 derived edges cross modules and name their witness"
                 (all (`elem` edges) want && length tops == 1)
                 ("edges: " <> show edges)
        _ -> report "D4 derived edges cross modules and name their witness" False "a probe module failed to parse"

-- | D5: a consumes entry that names a qualified function not among the
-- callers is reported; one that is a caller, or that is not a qualified
-- name, is not.
consumesDrift :: IO Bool
consumesDrift = do
  let idx = Map.fromList [(("M", "f"), [TestRef "X" "g" 1])]
      file consumes =
        (emptyChaseFile LangHaskell "M.hs")
          { chaseModuleName = "M"
          , chaseSignatures = [Signature "f" "f :: Int" 1]
          , chaseInvariants = [Invariant "f" ["reads the clock"] consumes Nothing]
          }
      ok     = checkConsumesDrift idx (file ["X.g (hot path)", "the last_seen_at column"])
      bad    = checkConsumesDrift idx (file ["X.h"])
      passed = null ok && length bad == 1 && "X.h" `isInfixOf` T.unpack (head bad)
      detail = "ok: " <> show ok <> " bad: " <> show bad
  report "D5 consumes is checked against callers for qualified names only" passed detail

report :: String -> Bool -> String -> IO Bool
report name ok detail = do
  putStrLn ((if ok then "PASS " else "FAIL ") <> name <> (if ok then "" else "\n     " <> detail))
  pure ok

main :: IO ()
main = do
  results <- sequence
    [ aliasInvariant, malformedFile, reproducibleBundle, discovery, coverageByModule
    , unwitnessedEdge, undeclaredEdge, noExit, derivedEdges, consumesDrift
    ]
  if and results then exitSuccess else exitFailure