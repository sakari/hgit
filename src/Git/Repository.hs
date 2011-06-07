{-# LANGUAGE NamedFieldPuns #-}
module Git.Repository (init, open, Repository, writeFile, withCRepository) where
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
import Git.Types
import qualified Data.ByteString as ByteString
import Prelude hiding (init, writeFile)
import System.FilePath

data Repository = Repository { repository_ptr::ForeignPtr C'git_repository }

newRepository::FilePath -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt) -> IO Repository  
newRepository path constructor = alloca $ \ptr_ptr -> withCString path $ \c'path ->
  constructor ptr_ptr c'path `wrap_git_result` wrap ptr_ptr
    where
      wrap ptr_ptr = do
        repo_ptr <- peek ptr_ptr
        liftM Repository $ repo_ptr `newForeignPtr` c'git_repository_free repo_ptr

withCRepository::Repository -> (Ptr C'git_repository -> IO a) -> IO a
withCRepository Repository { repository_ptr } c = withForeignPtr repository_ptr c

init::FilePath -> IO Repository
init path = newRepository path $ \pptr c'path -> c'git_repository_init pptr c'path 0
        
open::FilePath -> IO Repository
open path = newRepository path c'git_repository_open

workdir::Repository -> IO FilePath
workdir repo = withCRepository repo $ \c'repo -> 
  c'git_repository_workdir c'repo >>= peekCString

writeFile::Repository -> EntryName -> ByteString.ByteString -> IO ()
writeFile repo name contents = do
  repoPath <- workdir repo
  let absPath = repoPath </> entryName name
  absPath `ByteString.writeFile` contents 
