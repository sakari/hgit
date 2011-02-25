module Main where
import qualified Test.TestGit as TestGit
import Control.Monad
import System.Exit
import Test.QuickCheck

main = do
  results <- sequence TestGit.tests
  when (failuresIn results) $ exitWith $ ExitFailure 1

failuresIn results = all id $ map failed results
  where
    failed Failure {} = True
    failed _ = False 