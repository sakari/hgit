module Git.Commit where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String

import Git.Result
import Git.Repository
import Git.Oid
import Git.Tree

import Bindings.Libgit2

data Commit = Commit { commit_ptr::ForeignPtr C'git_commit 
                     , commit_repo_ptr::ForeignPtr C'git_repository
                     }

lookup::Repository -> Oid -> Result Commit
lookup repo oid = do
  alloca $ \ptr -> do
    withForeignPtr (repository_ptr repo) $ \repo_ptr -> do
      withForeignPtr (oid_ptr oid) $ \oid_ptr -> do
        c'git_commit_lookup ptr repo_ptr oid_ptr `handle_git_return` wrap ptr repo_ptr oid_ptr
  where
    wrap ptr repo_ptr oid_ptr = do
      wrapped_commit_ptr <- do 
        p <- peek ptr
        newForeignPtr p $ c'git_commit_close p
      return $ Commit { commit_ptr = wrapped_commit_ptr, commit_repo_ptr = repository_ptr repo }
  
type Ref = String
type Message = String
type OidT a = Oid
type Author = Signature
type Committer = Signature

data Signature = Signature { signature_ptr::ForeignPtr C'git_signature }
  
withMaybeCString::Maybe String -> (CString -> IO a) -> IO a
withMaybeCString string action = maybe (action nullPtr) (flip withCString action) string

withForeignPtrs::[ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs action = go fptrs [] 
  where
    go [] fptrs = action $ reverse fptrs
    go (a:as) fptrs = withForeignPtr a $ \fptr -> go as (fptr:fptrs) 

create::Repository -> Maybe Ref -> Author -> Committer -> Message -> OidT Tree -> [OidT Commit] -> Result (OidT Commit) 
create repo ref author committer message tree parents = do
  oid_fptr <- mallocForeignPtr
  withForeignPtr oid_fptr $ \result_oid_ptr -> do
    withForeignPtr (repository_ptr repo) $ \repo_ptr -> do
      withMaybeCString ref $ \ref_ptr -> do
        withForeignPtr (signature_ptr author) $ \auth_ptr -> do
          withForeignPtr (signature_ptr committer) $ \ committer_ptr -> do
            withCString message $ \message_ptr -> do
              withForeignPtr (oid_ptr tree) $ \tree_ptr -> do
                withForeignPtrs (map oid_ptr parents) $ \parent_oid_ptrs -> do
                  parent_oids <- mapM peek parent_oid_ptrs
                  withArray parent_oids $ \parent_oid_array -> do
                    c'git_commit_create result_oid_ptr repo_ptr ref_ptr auth_ptr committer_ptr message_ptr tree_ptr (fromIntegral $ length parents) parent_oid_array `handle_git_return` (return $ Oid oid_fptr)