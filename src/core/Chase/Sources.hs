{-# LANGUAGE MultiWayIf #-}

-- | The .hs and .purs files under a root, for compressing, for test
-- references and for callers alike.
--
-- Symbolic links found during the walk are not followed: a link to a
-- parent directory made the walk recurse until it ran out of stack or file
-- handles, and a link to another checkout put that checkout's modules in
-- this one's bundle. Directories whose name starts with a dot (.git,
-- .direnv, .inference) and build output (dist-newstyle, .stack-work,
-- .spago, output, node_modules) are not entered: run on a project root,
-- the walk used to compress dist-newstyle's generated Paths_ modules.
--
-- The root itself is taken as given, link or not, since naming it was a
-- choice. Files come out in sorted order, so a bundle's order depends only
-- on the tree.
module Chase.Sources
  ( findSourceFiles
  , isSourceFile
  , skippedDirectory
  ) where

import           Control.Monad    (forM)
import qualified Data.List
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import           System.FilePath  (takeExtension, (</>))

isSourceFile :: FilePath -> Bool
isSourceFile p = takeExtension p `elem` [".hs", ".purs"]

-- | A directory name the walk does not enter.
skippedDirectory :: FilePath -> Bool
skippedDirectory name =
  take 1 name == "."
    || name `elem` ["dist-newstyle", "dist", ".stack-work", ".spago", "output", "node_modules"]

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles root = do
  isFile <- doesFileExist root
  isDir  <- doesDirectoryExist root
  if
    | isFile    -> pure [root | isSourceFile root]
    | isDir     -> walk root
    | otherwise -> pure []
  where
    walk dir = do
      entries <- Data.List.sort <$> listDirectory dir
      fmap concat . forM entries $ \name -> do
        let p = dir </> name
        link <- pathIsSymbolicLink p
        sub  <- doesDirectoryExist p
        if
          | link -> pure []
          | sub && skippedDirectory name -> pure []
          | sub -> walk p
          | otherwise -> pure [p | isSourceFile p]