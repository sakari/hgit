module Test.Cases where
import qualified Test.Cases.Commit as Commit
import qualified Test.Cases.Oid as Oid
import qualified Test.Cases.Tree as Tree
import qualified Test.Cases.Index as Index

tests = [
  Commit.tests
  , Oid.tests
  , Tree.tests
  , Index.tests
  ]