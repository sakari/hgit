module Test.Cases where
import qualified Test.Cases.Commit as Commit
import qualified Test.Cases.Oid as Oid

tests = [Commit.tests, Oid.tests]