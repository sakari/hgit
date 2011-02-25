module Test.TestGit where
import qualified Git
import Test.QuickCheck
import Data.Maybe
import Data.String
import System.FilePath
import System.Posix.Directory
import System.Directory
import System.IO.Unsafe
import Control.Exception
import Control.Monad
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
        
given_a_repository block =                     
  sandbox $ bracket createRepo freeRepo block
    where
      createRepo = 
        fromJust `fmap` Git.initialize "repository" False
      freeRepo repo = Git.free repo

sandbox::IO Bool -> Bool
sandbox io = 
  unsafePerformIO $ 
    inSandboxDirectory "sandbox" io

newtype RepoName = RepoName FilePath 
                 deriving Show
                          
instance Arbitrary RepoName where
  arbitrary = RepoName `fmap` listOf1 (choose ('a', 'z'))
  
instance Arbitrary Git.Oid where  
  arbitrary = (fromJust . Git.oidMkStr) `fmap` vectorOf 40 (oneof [choose ('a', 'f'), choose('0', '9')])
  
instance Arbitrary Git.Blob where
  arbitrary = fromString `fmap` listOf1 arbitrary
  
instance Arbitrary Git.GitTime where  
  arbitrary = liftM2 Git.GitTime arbitrary arbitrary
  
instance Arbitrary Git.Signature where  
  arbitrary = liftM3 Git.Signature arbitrary arbitrary arbitrary
  
instance Arbitrary Git.Commit where
  arbitrary = do
    oid <- arbitrary
    msg <- arbitrary
    short_msg <- arbitrary
    author <- arbitrary
    committer <- arbitrary
    time <- arbitrary
    offset <- arbitrary
    tree <- arbitrary
    parents <- arbitrary
    return Git.Commit { Git.commit_id = oid
                      , Git.commit_message = msg
                      , Git.commit_short_message = short_msg
                      , Git.commit_time = time
                      , Git.commit_author = author
                      , Git.commit_committer = committer
                      , Git.commit_time_offset = offset
                      , Git.commit_tree = tree
                      , Git.commit_parents = parents
                      }

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

prop_lookup_nonexisting_commit oid =
  given_a_repository $ \repo -> do
    isNothing `fmap` Git.commitLookup repo oid
        
prop_oid_cpy oid = sandbox $ do
  (oid ==) `fmap` Git.oidCpy oid
  
prop_save_and_load_a_blob (blob::Git.Blob) =
  given_a_repository $ \repo -> do
    Just oid <- Git.write repo blob    
    Just blob_saved <- Git.lookup repo oid
    print $ show blob ++ "<>" ++ show blob_saved
    return $ blob_saved == blob
    
run title prop = do
  print $ "######### Test: " ++ title
  quickCheckResult prop

tests = [ run "open nonexisting repo" prop_opening_nonexistant_repo_fails
        , run "open initialized repo" prop_opening_an_initialized_repo_succeeds
        , run "initialize a repo" prop_initializing_a_normal_repository_succeeds
        , run "free created repo" prop_closing_an_initialized_repo
        , run "save and load a blob" prop_save_and_load_a_blob
        , run "lookup a non existing commit" prop_lookup_nonexisting_commit
        , run "oid contents stay the same during cpy" prop_oid_cpy
        ]