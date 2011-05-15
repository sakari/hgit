{-# LANGUAGE TemplateHaskell #-}
module Test.Cases.Commit where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.QuickCheck

import Test.Util
import qualified Git.Commit as Commit
import qualified Git.Oid as Oid

instance Arbitrary Oid.Oid where
  arbitrary = Oid.mkstr `fmap` vectorOf 40 arbitrary
  
prop_lookup_non_existing_commit oid = 
  with_repo $ \repo -> do
    fails $ Commit.lookup repo oid

tests = testGroup "Test.Cases.Commit" [ testProperty "lookup_non_existing_commit" prop_lookup_non_existing_commit]