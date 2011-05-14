module Git.Repository where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
import Bindings.Libgit2
import Control.Monad
import Git.Result

data Repository = Repository { repository_ptr::ForeignPtr C'git_repository }

open::String -> Result Repository
open path = alloca $ \ptr_ptr -> withCString path $ \c'path ->
  c'git_repository_open ptr_ptr c'path `handle_git_return` wrap ptr_ptr 
    where
      wrap ptr_ptr = do
        repo_ptr <- peek ptr_ptr
        liftM Repository $ repo_ptr `newForeignPtr` c'git_repository_free repo_ptr