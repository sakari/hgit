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
    shrink3 Git.TreeEntry tree_entry_oid tree_entry_filepath tree_entry_attributes
    
instance Arbitrary Git.Blob where
  arbitrary = fromString `fmap` listOf1 arbitrary
  
instance Arbitrary Git.GitTime where  
  arbitrary = liftM2 Git.GitTime arbitrary arbitrary
    
shrink3 c a1 a2 a3 = s1 ++ s2 ++ s3
  where
    s1 = [c sa1 a2 a3 | sa1 <- shrink a1 ]
    s2 = [c a1 sa2 a3 | sa2 <- shrink a2 ]
    s3 = [c a1 a2 sa3 | sa3 <- shrink a3 ]
  
    
anyString = listOf $ choose('\001', '\255')
    
instance Arbitrary Git.Signature where  
  arbitrary = liftM3 Git.Signature anyString anyString arbitrary
  shrink Git.Signature { Git.signature_name, Git.signature_email, Git.signature_time } = 
    shrink3 Git.Signature signature_name signature_email signature_time 
    
instance Arbitrary Git.Commit where
  arbitrary = liftM5 Git.Commit anyString arbitrary arbitrary arbitrary $ return []
  shrink (Git.Commit a b c d e) = shrink5 Git.Commit a b c d e
  
shrink5 c a1 a2 a3 a4 a5 = s1 ++ s2 ++ s3 ++ s4 ++ s5
  where
    s1 = [c sa1 a2 a3 a4 a5 | sa1 <- shrink a1 ]
    s2 = [c a1 sa2 a3 a4 a5 | sa2 <- shrink a2 ]
    s3 = [c a1 a2 sa3 a4 a5 | sa3 <- shrink a3 ]
    s4 = [c a1 a2 a3 sa4 a5 | sa4 <- shrink a4 ]
    s5 = [c a1 a2 a3 a4 sa5 | sa5 <- shrink a5 ]


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
    
prop_lookup_nonexisting_commit oid =
  given_a_repository $ \repo -> do
    (isNothing::Maybe Git.Commit -> Bool) `fmap` Git.lookup repo oid

prop_save_and_load_a_commit (commit::Git.Commit) (parents::[Git.Commit]) =
  given_a_repository $ \repo -> do
    parentOids <- forM parents $ Git.write repo 
    let originalCommit = commit { Git.commit_parents = map fromJust parentOids }
    Just oid <- Git.write repo originalCommit
    Just savedCommit <- Git.lookup repo oid
    when (originalCommit /= savedCommit) $ print $ "saved: " ++ show savedCommit ++ "<>" ++ show originalCommit
    return $ originalCommit == savedCommit
        
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
        , run "save and load a commit" prop_save_and_load_a_commit
        ]