module Test.Cases.Tree where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.QuickCheck
import Test.Util
import qualified Git.Tree as Tree

tests = testGroup "Test.Cases.Tree" 
        [ testProperty "lookup non existing tree" $ \oid -> 
           with_repo $ \repo -> do
             fails $ Tree.lookup repo oid
        , testProperty "write and lookup a tree" $ \tree -> 
           with_repo $ \repo -> do
             storedOid <- Tree.write repo tree
             foundTree <- Tree.lookup repo storedOid
             return True             
        ]



