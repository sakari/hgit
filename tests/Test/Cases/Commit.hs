{-# LANGUAGE TemplateHaskell #-}
module Test.Cases.Commit where
import Test.Framework.Providers.QuickCheck2
import Test.Framework

import Test.Util
import qualified Git.Commit as Commit
    
tests::Test
tests = testGroup "Test.Cases.Commit" [ testProperty "lookup non existing commit" $ \oid ->
                                         with_repo $ \repo -> do
                                           fails $ Commit.lookup repo oid

                                      , testProperty "lookup an existing commit" $ \author committer treeOid parentOids -> 
                                         with_repo $ \repo -> do
                                           oid <- Commit.create repo Nothing  author committer "message" treeOid parentOids 
                                           foundCommit <- Commit.lookup repo oid
                                           return $ Commit.author foundCommit == author &&
                                             Commit.committer foundCommit == committer &&
                                             Commit.tree foundCommit == treeOid &&
                                             Commit.parents foundCommit == parentOids
                                      ]