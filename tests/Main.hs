module Main where

import System.Process
import System.Exit
import Control.Monad hiding (unless)
import System.Directory
import System.FilePath

unless c False = c 
unless c _ = return ()

quicktest filepath = do
  (exit, stdout, stderr) <- readProcessWithExitCode "quicktest" [filepath] ""
  putStr stdout `unless` null stdout 
  putStr stderr `unless` null stderr 
  return exit

get_all_hs_files = go "tests"
  where
    is_dotpath path = path == ".." || path == "." 
    go cwd = do
      relative_contents <- filter (not . is_dotpath) `fmap` getDirectoryContents cwd
      let contents = combine cwd `map` relative_contents
          local_sources = filter is_haskell_file contents
      dirs <- filterM doesDirectoryExist contents
      ((local_sources ++) . concat) `fmap` mapM go dirs
    is_haskell_file file = takeExtension file == ".hs" || takeExtension file == ".lhs"     

main = do
  test_files <- get_all_hs_files
  let failure ExitSuccess = False
      failure (ExitFailure _) = True
  failed <- any failure `fmap` mapM quicktest test_files
  if failed then exitFailure
    else exitSuccess
