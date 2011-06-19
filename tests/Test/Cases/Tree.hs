module Test.Cases.Tree where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.QuickCheck
import Test.Util
import qualified Git.Oid as Oid
import qualified Git.Tree as Tree
import qualified Git.Types as Types
import qualified Git.TreeBuilder as Builder
import qualified Data.Map as Map
import Data.List (sort)

tests::Test
tests = testGroup "Test.Cases.Tree" 
        [ 
          testProperty "lookup non existing tree" $ \oid -> 
           with_repo $ \repo -> do
             fails $ Tree.lookup repo oid
        
        , testProperty "write and lookup a tree" $ \paths ->
           with_repo $ \repo -> do
             let tree = Map.fromList paths
             storedOid <- Tree.write repo tree
             _ <- Tree.lookup repo storedOid
             return True
          
        , testProperty "return Nothing if no entry with the given name" $ \path -> withStoredTree $ \paths tree -> do
             r <- Tree.entry tree path
             return $ r == Nothing || path `elem` map fst paths
             
        , testProperty "get entry by name" $ \path entryOid entryAttr restOfPaths -> do  
             let allPaths = restOfPaths ++ [(path, (entryOid, entryAttr))]
             flip withStoredTree allPaths $ \_ tree -> do
               entry <- Tree.entry tree path
               (Just $ Types.TreeEntry path entryOid entryAttr) `assertEqual` entry 
        
        , testProperty "list all tree entries" $ withStoredTree $ \paths tree -> do
             entries <- Tree.entries tree
             let go (name, (oid, attr)) = Types.TreeEntry name oid attr
             sort entries `assertEqual` (sort $ map go paths)
             
        , testProperty "Finding entry 'b' succeeds (https://github.com/libgit2/libgit2/issues/127)" $ with_repo $ \repo -> do
             let zeroOid = Oid.mkstr $ replicate 40 '0' 
                 name = Types.unsafePathToEntry "b"
                 attrs = Types.Attributes 0
                 target = Types.TreeEntry name zeroOid attrs                 
                 directory = Types.Attributes 16384
                 haystack = Map.fromList [(name, (zeroOid, attrs)) 
                                         , (Types.unsafePathToEntry "ba", (zeroOid, directory))
                                         , (Types.unsafePathToEntry "a", (zeroOid, Types.Attributes 0))
                                         ]
             oid <- Tree.write repo haystack
             tree <- Tree.lookup repo oid
             entry <- Tree.entry tree name
             Just target `assertEqual` entry
                     
        , testProperty "removing non existing entry fails" $ \name entries -> with_repo $ \_ -> do
             builder <- Builder.create
             mapM_ (Builder.insert builder) $ filter ((name /=) . Types.treeEntryName) entries
             fails $ Builder.remove builder name

        , testProperty "remove entry from treebuilder" $ \entries_l remove entries_r -> with_repo $ \repo -> do
             let entries = removeName entries_l ++ [remove] ++ removeName entries_r
                 name = Types.treeEntryName remove
                 removeName = filter ((name /=) . Types.treeEntryName)
             builder <- Builder.create
             Builder.insert builder `mapM_` entries
             Builder.remove builder name
             oid <- Builder.write repo builder
             tree <- Tree.lookup repo oid
             entry <- Tree.entry tree name
             Nothing `assertEqual` entry
             
        , testProperty "Bug: Removing entry fails" $ with_repo $ \_ -> do
             let zeroOid = Oid.mkstr $ replicate 40 '0' 
                 name = Types.unsafePathToEntry "b"
                 attrs = Types.Attributes 0
                 directory = Types.Attributes 16384
                 haystack = [ Types.TreeEntry name zeroOid attrs 
                            , Types.TreeEntry (Types.unsafePathToEntry "ba") zeroOid  directory
                            , Types.TreeEntry (Types.unsafePathToEntry "a") zeroOid (Types.Attributes 0)
                            ]
             builder <- Builder.create
             Builder.insert builder `mapM_` haystack
             fails $ Builder.remove builder name
        ]
        
type Paths = [(Types.EntryName, (Oid.Oid, Types.Attributes))]        
withStoredTree::Testable a => (Paths -> Tree.Tree -> IO a) -> Paths -> Property
withStoredTree c paths = with_repo $ \repo -> do 
  let tree = Map.fromList paths
      uniquePaths = Map.toList tree
  storedOid <- Tree.write repo tree
  foundTree <- Tree.lookup repo storedOid
  c uniquePaths foundTree

