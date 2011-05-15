module Git.Repository (init, open, Repository(..)) where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Bindings.Libgit2
import Control.Monad
import Git.Result
import Prelude hiding (init)

data Repository = Repository { repository_ptr::ForeignPtr C'git_repository }

newRepository::FilePath -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt) -> Result Repository  
newRepository path constructor = alloca $ \ptr_ptr -> withCString path $ \c'path ->
  constructor ptr_ptr c'path `handle_git_return` wrap ptr_ptr
    where
      wrap ptr_ptr = do
        repo_ptr <- peek ptr_ptr
        liftM Repository $ repo_ptr `newForeignPtr` c'git_repository_free repo_ptr

init::FilePath -> Result Repository
init path = newRepository path $ \pptr c'path -> c'git_repository_init pptr c'path 0
        
open::FilePath -> Result Repository
open path = newRepository path c'git_repository_open
