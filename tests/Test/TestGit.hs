module Test.TestGit where
import qualified Git
import Test.QuickCheck
import Data.Maybe
import System.FilePath
import System.Posix.Directory
import System.Directory
import System.IO.Unsafe
import Control.Exception
import Prelude hiding (catch)

inSandboxDirectory::FilePath -> IO a -> IO a
inSandboxDirectory sandboxLocation io = bracket enterSandbox exitSandbox (const io) 
  where
    createParents = True
    enterSandbox = do
      previousWD <- getWorkingDirectory
      createDirectoryIfMissing createParents sandboxLocation
      changeWorkingDirectory sandboxLocation
      return previousWD
    exitSandbox previousWD = do
      changeWorkingDirectory previousWD
      removeDirectoryRecursive sandboxLocation
          
exceptionToFalse::SomeException -> IO Bool
exceptionToFalse _ = return False
        
sandbox::IO Bool -> Bool
sandbox io = 
  unsafePerformIO $ 
    inSandboxDirectory "sandbox" $
      io `catch` exceptionToFalse

newtype RepoName = RepoName FilePath 
                 deriving Show
instance Arbitrary RepoName where
  arbitrary = RepoName `fmap` listOf1 (choose ('a', 'z'))
  
prop_opening_nonexistant_repo_fails (RepoName repo) =
  sandbox $ isNothing `fmap` Git.open repo

prop_initializing_a_normal_repository_succeeds (RepoName repo) = 
  sandbox $ isJust `fmap` Git.initialize repo False 

prop_closing_an_initialized_repo (RepoName name) =
  sandbox $ do
    Just repo <- Git.initialize name False 
    Git.free repo
    return True

prop_opening_an_initialized_repo_succeeds (RepoName name) =
  sandbox $ do
    Just repo <- Git.initialize name False
    isJust `fmap` Git.open (joinPath [name, ".git"]) 

run title prop = do
  print $ "######### Test: " ++ title
  quickCheckResult prop

tests = [run "open nonexisting repo" prop_opening_nonexistant_repo_fails
        , run "open initialized repo" prop_opening_an_initialized_repo_succeeds
        , run "initialize a repo" prop_initializing_a_normal_repository_succeeds
        , run "free created repo" prop_closing_an_initialized_repo
        ]