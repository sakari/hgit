module Test.TestGit where
import qualified Git
import Test.QuickCheck
import Data.Maybe
import System.FilePath
import System.IO.Unsafe

sandbox::IO Bool -> Bool
sandbox io = unsafePerformIO $ io `catch` (const $ return False)

newtype Path = Path FilePath
             deriving Show

instance Arbitrary Path where
  arbitrary = (Path . joinPath) `fmap` listOf1 pathFragment
    where
      pathFragment = oneof [return "."
                           , listOf $ choose ('a', 'z')]

prop_opening_nonexistant_repo_fails (Path path) =
  sandbox $ isNothing `fmap` Git.open path

tests = [quickCheckResult prop_opening_nonexistant_repo_fails]