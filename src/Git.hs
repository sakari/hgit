module Git (Repository, open) where
import Bindings.Libgit2
import Foreign
import Foreign.C.String
import Control.Monad(when)

newtype Repository = Repository (Ptr C'git_repository)

open::FilePath -> IO (Maybe Repository)
open path = alloca $ \repoPtr -> do
  withCString path $ \pathPtr -> do
    r <- c'git_repository_open repoPtr pathPtr
    if  (r < 0) then return Nothing
      else (Just . Repository) `fmap` peek repoPtr