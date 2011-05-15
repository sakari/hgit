module Test.Cases.Tree where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.QuickCheck
import Test.Util

import qualified Git.Tree as Tree
import qualified Git.Types as Types
import Control.Arrow
import qualified Data.Map as Map

tests = testGroup "Test.Cases.Tree" 
        [ 
          testProperty "lookup non existing tree" $ \oid -> 
           with_repo $ \repo -> do
             fails $ Tree.lookup repo oid
        
        , testProperty "write and lookup a tree" $ \paths -> 
           with_repo $ \repo -> do
             let tree = Map.fromList paths
             storedOid <- Tree.write repo tree
             foundTree <- Tree.lookup repo storedOid
             entries <- mapM (Tree.entry foundTree . fst) paths
             return $ tree == (Map.fromList $ map (Types.treeEntryName &&& Types.treeEntryOid) entries)
        ]



