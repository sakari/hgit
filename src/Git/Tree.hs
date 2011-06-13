{-# LANGUAGE NamedFieldPuns #-}

module Git.Tree (Tree
                , lookup
                , write
                , fromIndex
                , entry
                , entries
                ) where
import Bindings.Libgit2
import Foreign.ForeignPtr
import Foreign.Ptr
import Git.Object
import Git.Repository hiding (init)
import Git.Internal.Repository
import Git.Oid
import Git.Internal.Oid
import Git.Result
import qualified Git.TreeBuilder as Builder
import Git.Types 
import Git.Index hiding (write)
import Control.Monad
import Control.Applicative
import Foreign.C.String
import Foreign.Marshal.Alloc
import Prelude hiding (lookup)
import qualified Data.Map as Map

data Tree = Tree { tree_ptr::ForeignPtr C'git_tree }

lookup::WithAnyRepository repo => repo -> Oid -> IO Tree
lookup repo oid = lookup_wrapped_object repo oid Tree c'GIT_OBJ_TREE

-- | Write index as a tree to the object store

fromIndex::Index -> IO Oid
fromIndex index = withCIndex index $ \c'index ->
  alloca $ \c'oid -> do 
    c'git_tree_create_fromindex c'oid c'index `wrap_git_result` fromCOid c'oid 
  
write::WithAnyRepository repo => repo -> Map.Map EntryName (Oid, Attributes) -> IO Oid
write repo paths = do
  builder <- Builder.create
  let build k (oid, attr) = TreeEntry k oid attr 
  mapM_ (Builder.insert builder) $ Map.elems $ Map.mapWithKey build paths
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

entry::Tree -> EntryName -> IO (Maybe TreeEntry)
entry tree name  = withCTree tree $ \c'tree ->
   withCString (entryToPath name) $ \c'path -> do
     c'entry <- c'git_tree_entry_byname c'tree c'path
     if c'entry == nullPtr then return Nothing
       else Just `fmap` fromCEntry c'entry
     
fromCEntry::Ptr C'git_tree_entry -> IO TreeEntry
fromCEntry c'entry = liftM3 TreeEntry (fmap unsafePathToEntry $ c'git_tree_entry_name c'entry >>= peekCString) 
                     (c'git_tree_entry_id c'entry >>= fromCOid)
                     (fmap fromIntegral $ c'git_tree_entry_attributes c'entry)
