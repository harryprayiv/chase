{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- | Where a project keeps the files for its LLM tooling, and how chase
-- finds them.
--
-- A project keeps everything for the LLM tooling in one directory at its
-- root, @.inference@: chase's annotations, open-slop's .llmqignore and
-- .llmqchase, and bundles kept for reference. open-slop reads the same
-- directory, so a repository set up for one is set up for the other.
--
-- The project is found from the source being compressed, never from the
-- working directory. chase used to look in the working directory first,
-- and open-slop recorded the result on 2026-10-17: run from chase's own
-- checkout, chase attached chase's annotations to open-slop's modules.
-- Walking up from the source finds the project the source belongs to,
-- wherever chase is started.
module Chase.Inference
  ( inferenceDirName
  , annotationFileNames
  , unisonAnnotationFileName
  , findProjectRoot
  , locateAnnotations
  , defaultAnnotationsTarget
  ) where

import Control.Monad    (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, makeAbsolute)
import System.FilePath  (takeDirectory, (</>))

-- | The directory, at a project's root, that holds the LLM tooling's files.
inferenceDirName :: FilePath
inferenceDirName = ".inference"

-- | The names chase reads its annotations from, in the order tried.
annotationFileNames :: [FilePath]
annotationFileNames = ["chase-annotation.json", "chase-annotations.json"]

-- | The annotations file chase-unison reads by default, inside .inference.
unisonAnnotationFileName :: FilePath
unisonAnnotationFileName = "chase-unison-annotations.json"

-- | The nearest directory at or above the given path that has a
-- .inference directory. The walk stops at the first directory holding
-- .git, so it never leaves the repository the path is in: a checkout
-- without .inference has no project root, whatever its parent holds.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot path = do
  absolute <- makeAbsolute path
  isDir    <- doesDirectoryExist absolute
  go (if isDir then absolute else takeDirectory absolute)
  where
    go dir = do
      hasInference <- doesDirectoryExist (dir </> inferenceDirName)
      hasGit       <- doesPathExist (dir </> ".git")
      let parent = takeDirectory dir
      if
        | hasInference -> pure (Just dir)
        | hasGit || parent == dir -> pure Nothing
        | otherwise -> go parent

-- | The annotations file for the project the path belongs to: the first of
-- 'annotationFileNames' that exists in its .inference directory.
locateAnnotations :: FilePath -> IO (Maybe FilePath)
locateAnnotations path =
  findProjectRoot path >>= \case
    Nothing   -> pure Nothing
    Just root -> do
      found <- filterM doesFileExist [root </> inferenceDirName </> n | n <- annotationFileNames]
      pure (case found of
        (f : _) -> Just f
        []      -> Nothing)

-- | Where chase-annotate writes when no output is given: the project's
-- existing annotations file, or chase-annotations.json in its .inference
-- directory. Nothing when the path is in no project with a .inference
-- directory.
defaultAnnotationsTarget :: FilePath -> IO (Maybe FilePath)
defaultAnnotationsTarget path =
  locateAnnotations path >>= \case
    Just f  -> pure (Just f)
    Nothing -> fmap (\root -> root </> inferenceDirName </> "chase-annotations.json") <$> findProjectRoot path
