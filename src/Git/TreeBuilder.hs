{-# LANGUAGE NamedFieldPuns #-}

module Git.TreeBuilder where
import Git.Oid
import Git.Repository
import Git.Result

import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Concurrent 
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.C.String

data TreeBuilder = TreeBuilder { treeBuilder::ForeignPtr C'git_treebuilder }

create::IO TreeBuilder
create = alloca $ \ptr -> c'git_treebuilder_create ptr nullPtr `wrap_git_result` (peek ptr >>= go)
  where
    go ptr = fmap TreeBuilder $ newForeignPtr ptr $ c'git_treebuilder_free ptr

insert::TreeBuilder -> FilePath -> Oid -> IO ()
insert TreeBuilder { treeBuilder } path Oid { oid_ptr } = withForeignPtr treeBuilder $ \c'builder ->  
  withCString path $ \c'path -> do
    withForeignPtr oid_ptr $ \c'oid -> do
      c'git_treebuilder_insert nullPtr c'builder c'path c'oid 0 `wrap_git_result` return ()

write::Repository -> TreeBuilder -> IO Oid 
write Repository { repository_ptr } TreeBuilder { treeBuilder } = do
  withForeignPtr repository_ptr $ \c'repository ->
    withForeignPtr treeBuilder $ \c'builder -> do
      foidPtr <- mallocForeignPtr
      withForeignPtr foidPtr $ \oidPtr ->
        c'git_treebuilder_write oidPtr c'repository c'builder `wrap_git_result` return (Oid foidPtr)
    

