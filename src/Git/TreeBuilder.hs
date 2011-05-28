{-# LANGUAGE NamedFieldPuns #-}

module Git.TreeBuilder (TreeBuilder
                       , create
                       , insert
                       , write
                       , remove
                       ) where
import Git.Oid
import Git.Repository
import Git.Result
import Git.Types

import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Concurrent 
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.C.String

data TreeBuilder = TreeBuilder { treeBuilder::ForeignPtr C'git_treebuilder }

withCTreeBuilder::TreeBuilder -> (Ptr C'git_treebuilder -> IO a) -> IO a
withCTreeBuilder TreeBuilder { treeBuilder } = withForeignPtr treeBuilder

remove::TreeBuilder -> EntryName -> IO ()
remove treeBuilder EntryName { entryName } = withCTreeBuilder treeBuilder $ \c'builder ->
  withCString entryName $ \c'entry -> 
  c'git_treebuilder_remove c'builder c'entry `wrap_git_result` return ()

create::IO TreeBuilder
create = alloca $ \ptr -> c'git_treebuilder_create ptr nullPtr `wrap_git_result` (peek ptr >>= go)
  where
    go ptr = fmap TreeBuilder $ newForeignPtr ptr $ c'git_treebuilder_free ptr

insert::TreeBuilder -> TreeEntry -> IO ()
insert TreeBuilder { treeBuilder } TreeEntry { treeEntryName, treeEntryOid, treeEntryAttributes } = 
  withForeignPtr treeBuilder $ \c'builder -> do 
    withCString (entryName treeEntryName) $ \c'path -> do
      withCOid treeEntryOid $ \c'oid -> do
        c'git_treebuilder_insert nullPtr c'builder c'path c'oid (fromIntegral treeEntryAttributes) `wrap_git_result` return ()

write::Repository -> TreeBuilder -> IO Oid 
write repo TreeBuilder { treeBuilder } = do
  withCRepository repo $ \c'repository ->
    withForeignPtr treeBuilder $ \c'builder -> do
      alloca $ \oidPtr ->
        c'git_treebuilder_write oidPtr c'repository c'builder `wrap_git_result` fromCOid oidPtr
    

