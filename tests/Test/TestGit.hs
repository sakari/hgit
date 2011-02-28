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
  
instance Arbitrary Git.Tree where
  arbitrary = liftM Git.Tree $ listOf1 arbitrary
  shrink = fmap Git.Tree . shrink . Git.tree_entries 

newtype Path = Path { fromPath::FilePath }
instance Arbitrary Path where
  arbitrary = fmap Path $ listOf validPathChars
    where
      -- this is certainly too narrow set
      validPathChars = choose('a', 'z') 
  shrink = fmap Path . shrink . fromPath
  
  
newtype FileMode = FileMode { fromFileMode::Int }
                 deriving (Eq, Ord, Show, Num)  
                          
instance Arbitrary FileMode where
  arbitrary = fmap FileMode $ choose(0, 0o777777)
  shrink n | n > 0 = [n - 1]
           | otherwise = [] 
    
instance Arbitrary Git.TreeEntry where
  arbitrary = liftM3 Git.TreeEntry arbitrary (fmap fromPath arbitrary) (fmap fromFileMode arbitrary)
  shrink Git.TreeEntry { Git.tree_entry_oid, Git.tree_entry_filepath, Git.tree_entry_attributes } = 
    shrunk_oid ++ shrunk_path ++ shrunk_attributes
    where
      shrunk_oid = [Git.TreeEntry oid tree_entry_filepath tree_entry_attributes | oid <- shrink tree_entry_oid]
      shrunk_path = [Git.TreeEntry tree_entry_oid path tree_entry_attributes | path <- shrink tree_entry_filepath]
      shrunk_attributes = [Git.TreeEntry tree_entry_oid tree_entry_filepath attrs | attrs <- shrink tree_entry_attributes]
    
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
    return $ blob_saved == blob
    
prop_save_and_load_a_tree (tree::Git.Tree) =    
  given_a_repository $ \repo -> do
    Just oid <- Git.write repo tree
    Just tree_saved <- Git.lookup repo oid
    return $ tree_saved == tree    
    
    
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
        , run "save and load a tree" prop_save_and_load_a_tree
        ]