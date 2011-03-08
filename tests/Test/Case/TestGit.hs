module Test.Case.TestGit where
import qualified Git
import Test.QuickCheck
import Data.Maybe
import Control.Monad
import Prelude hiding (catch)
import Test.Util
import Test.Arbitrary
import System.FilePath


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
    return $ originalCommit == savedCommit

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