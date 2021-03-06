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

data Object = Object { object_ptr::ForeignPtr C'git_object }

instance Functor (Either a) where
  fmap f (Right r) = Right $ f r
  fmap f (Left l) = Left l

lookup_wrapped_object::Repository -> Oid -> (ForeignPtr a -> b) -> C'git_otype -> Result b 
lookup_wrapped_object repo oid wrapper otype = 
  (fmap . fmap) (wrapper . castForeignPtr . object_ptr) $ 
  lookup_any repo oid otype

lookup_any::Repository -> Oid -> C'git_otype -> Result Object
lookup_any repo oid otype = do
  alloca $ \ptr -> do
    withForeignPtr (repository_ptr repo) $ \repo_ptr -> do
      withForeignPtr (oid_ptr oid) $ \oid_ptr -> do
        c'git_object_lookup ptr repo_ptr oid_ptr otype `handle_git_return` wrap ptr repo_ptr oid_ptr
  where
    wrap ptr repo_ptr oid_ptr = do
      p <- peek ptr
      fmap Object $ newForeignPtr p $ c'git_object_close p
