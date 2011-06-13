module Git.Object where
import Bindings.Libgit2
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Storable 
import Foreign.Marshal.Alloc
import Git.Internal.Result
import Git.Repository
import Git.Internal.Repository
import Git.Oid
import Git.Internal.Oid
import Control.Monad.Instances

data Object = Object { object_ptr::ForeignPtr C'git_object }

-- | Look up objects based on the git type and wrap the returned as 'b'

lookup_wrapped_object::WithAnyRepository repo => repo -> Oid -> (ForeignPtr a -> b) -> C'git_otype -> IO b 
lookup_wrapped_object repo oid wrapper otype = 
  fmap (wrapper . castForeignPtr . object_ptr) $ 
  lookup_any repo oid otype

-- | Look up objects 

lookup_any::WithAnyRepository repo => repo -> Oid -> C'git_otype -> IO Object
lookup_any repo oid otype = do
  alloca $ \ptr -> do
    withCFAnyRepository repo $ \f'repo -> do 
      withForeignPtr f'repo $ \c'repo -> do
        withCOid oid $ \oid_ptr -> do
          c'git_object_lookup ptr c'repo oid_ptr otype `wrap_git_result` wrap ptr f'repo oid_ptr
  where
    wrap ptr f'repo oid_ptr = do
      p <- peek ptr
      fmap Object $ newForeignPtr p $ c'git_object_close p >> touchForeignPtr f'repo
