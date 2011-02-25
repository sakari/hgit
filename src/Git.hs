module Git (Repository
           , Commit
           , Oid
           , oidMkStr
           , open
           , initialize
           , free
           , commitLookup) where
import Bindings.Libgit2
import Foreign hiding (free)
import Foreign.C.String
import Foreign.C.Types
import Control.Monad(when)

newtype Commit = Commit { commitPtr::Ptr C'git_commit }
newtype Oid = Oid { oidPtr::Ptr C'git_oid }
newtype Repository = Repository { repoPtr :: Ptr C'git_repository }

instance Show Oid where
  show oid = "{ Oid: " ++ oidFmt oid ++ " }"

oidFmt::Oid -> String
oidFmt Oid { oidPtr } = unsafePerformIO $ do
  allocaBytes c'GIT_OID_HEXSZ $ \output -> do 
    c'git_oid_fmt output oidPtr
    peekCStringLen (output, c'GIT_OID_HEXSZ)

oidMkStr::String -> Maybe Oid
oidMkStr hex 
  | length hex /= 40 = Nothing
  | otherwise = unsafePerformIO $ do
    withCString hex $ \c'hex -> do
      oidPtr <- malloc
      r <- c'git_oid_mkstr oidPtr c'hex 
      if (r < 0) then return Nothing
        else return $ Just $ Oid oidPtr

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

free::Repository -> IO ()
free = c'git_repository_free . repoPtr

commitLookup::Repository -> Oid -> IO (Maybe Commit)
commitLookup Repository { repoPtr } Oid { oidPtr } = do
  alloca $ \commitPtr -> do
    r <- c'git_commit_lookup commitPtr repoPtr oidPtr
    if (r < 0) then return Nothing
      else (Just . Commit) `fmap` peek commitPtr 

c'bool::Bool -> CInt
c'bool True = 1
c'bool False = 0
           