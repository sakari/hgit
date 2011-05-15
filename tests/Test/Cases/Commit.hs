{-# LANGUAGE TemplateHaskell #-}
module Test.Cases.Commit where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.QuickCheck

import Test.Util
import qualified Git.Commit as Commit
import qualified Git.Oid as Oid

prop_lookup_non_existing_commit oid = 
  with_repo $ \repo -> do
    fails $ Commit.lookup repo oid
    
prop_lookup_existing_commit author committer treeOid parentOids =
  with_repo $ \repo -> do
    oid <- success $ Commit.create repo Nothing  author committer "message" treeOid parentOids 
    foundCommit <- success $ Commit.lookup repo oid
    return True
    
tests = testGroup "Test.Cases.Commit" [ testProperty "lookup non existing commit" prop_lookup_non_existing_commit
                                      -- , testProperty "lookup an existing commit" prop_lookup_existing_commit
                                      ]