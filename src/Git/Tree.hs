{-# LANGUAGE NamedFieldPuns #-}

module Git.Tree (Tree
                , lookup, write
                , entry
                , entries
                ) where
import Bindings.Libgit2
import Foreign.ForeignPtr
import Foreign.Ptr
import Git.Object
import Git.Repository hiding (init)
import Git.Oid
import Git.Result
import qualified Git.TreeBuilder as Builder
import Git.Types
import Control.Monad
import Control.Applicative
import Foreign.C.String
import Prelude hiding (lookup)
import qualified Data.Map as Map

data Tree = Tree { tree_ptr::ForeignPtr C'git_tree }

lookup::Repository -> Oid -> IO Tree
lookup repo oid = lookup_wrapped_object repo oid Tree c'GIT_OBJ_TREE

write::Repository -> Map.Map EntryName Oid -> IO Oid
write repo paths = do
  builder <- Builder.create
  mapM_ (Builder.insert builder . uncurry TreeEntry) $ Map.toList $ paths
  Builder.write repo builder 

withCTree::Tree -> (Ptr C'git_tree -> IO a) -> IO a
withCTree Tree { tree_ptr } c = withForeignPtr tree_ptr c

excludeUpperBound [] = []
excludeUpperBound a = init a

entries::Tree -> IO [TreeEntry]
entries tree = withCTree tree $ \c'tree -> do
  entryCount <- c'git_tree_entrycount c'tree
  mapM (go c'tree) $ excludeUpperBound [0 .. entryCount] 
    where
      go c'tree index = c'git_tree_entry_byindex c'tree (fromIntegral index) >>= fromCEntry

entry::Tree -> EntryName -> IO TreeEntry
entry tree EntryName { entryName } = withCTree tree $ \c'tree ->
   withCString entryName $ \c'path -> c'git_tree_entry_byname c'tree c'path >>= fromCEntry
     
fromCEntry::Ptr C'git_tree_entry -> IO TreeEntry
fromCEntry c'entry = liftM2 TreeEntry (fmap EntryName $ c'git_tree_entry_name c'entry >>= peekCString) 
       (c'git_tree_entry_id c'entry >>= fromCOid)
       -- todo: entry attributes| (fmap fromIntegral $ c'git_tree_entry_attributes c'entry)
