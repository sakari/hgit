module Git.Object where
import Bindings.Libgit2
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Storable 
import Foreign.Marshal.Alloc
import Git.Result
import Git.Repository
import Git.Oid
import Control.Monad.Instances

data Object = Object { object_ptr::ForeignPtr C'git_object }

lookup_wrapped_object::Repository -> Oid -> (ForeignPtr a -> b) -> C'git_otype -> IO b 
lookup_wrapped_object repo oid wrapper otype = 
  fmap (wrapper . castForeignPtr . object_ptr) $ 
  lookup_any repo oid otype

lookup_any::Repository -> Oid -> C'git_otype -> IO Object
lookup_any repo oid otype = do
  alloca $ \ptr -> do
    withForeignPtr (repository_ptr repo) $ \repo_ptr -> do
      withCOid oid $ \oid_ptr -> do
        c'git_object_lookup ptr repo_ptr oid_ptr otype `wrap_git_result` wrap ptr repo_ptr oid_ptr
  where
    wrap ptr repo_ptr oid_ptr = do
      p <- peek ptr
      fmap Object $ newForeignPtr p $ c'git_object_close p
