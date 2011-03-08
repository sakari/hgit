module Main where
import qualified Test.Case.TestGit as TestGit
import qualified Test.Case.Trace as Trace
import Control.Monad
import System.Exit
import Test.QuickCheck

main = do
  results <- sequence $ TestGit.tests ++ Trace.tests
  when (failuresIn results) $ exitWith $ ExitFailure 1

failuresIn results = any id $ map failed results
  where
    failed Failure {} = True
    failed _ = False 