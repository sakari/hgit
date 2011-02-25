module Git (Repository
           , open
           , initialize) where
import Bindings.Libgit2
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad(when)

newtype Repository = Repository (Ptr C'git_repository)

open::FilePath -> IO (Maybe Repository)
open path = alloca $ \repoPtr -> do
  withCString path $ \pathPtr -> do
    r <- c'git_repository_open repoPtr pathPtr
    if  (r < 0) then return Nothing
      else (Just . Repository) `fmap` peek repoPtr
           
initialize::FilePath -> Bool -> IO (Maybe Repository)
initialize path is_bare = alloca $ \repoPtr -> do
  withCString path $ \pathPtr -> do
    r <- c'git_repository_init repoPtr pathPtr $ c'bool is_bare
    if (r < 0) then return Nothing
      else (Just . Repository) `fmap` peek repoPtr

c'bool::Bool -> CInt
c'bool True = 1
c'bool False = 0
           