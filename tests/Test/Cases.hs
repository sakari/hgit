module Test.Cases where
import qualified Test.Cases.Commit
import qualified Test.Cases.Oid
import qualified Test.Cases.Tree
import qualified Test.Cases.Index
import qualified Test.Cases.Blob
import Test.Framework

tests::[Test]
tests = [
  Test.Cases.Commit.tests
  , Test.Cases.Oid.tests
  , Test.Cases.Tree.tests
  , Test.Cases.Index.tests
  , Test.Cases.Blob.tests
  ]