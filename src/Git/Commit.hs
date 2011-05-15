module Git.Commit where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String

import Data.Word

import Git.Result
import Git.Repository
import Git.Oid
import Git.Tree
import Git.Object

import Bindings.Libgit2

data Commit = Commit { commit_ptr::ForeignPtr C'git_commit 
                     , commit_repo_ptr::ForeignPtr C'git_repository
                     }

lookup::Repository -> Oid -> Result Commit
lookup repo oid = lookup_wrapped_object repo oid wrap c'GIT_OBJ_COMMIT
  where
    wrap fptr = Commit { commit_ptr = fptr, commit_repo_ptr = repository_ptr repo}
  
type Ref = String
type Message = String
type OidT a = Oid
type Author = Signature
type Committer = Signature

data Time = Time { time_epoch::Word, time_offset::Word }
          deriving Show
data Signature = Signature { signature_author::String, signature_email::String, signature_time::Time }
               deriving Show

with_signature::Signature -> (C'git_signature -> Result a) -> Result a
with_signature sig action = do 
  withCString (signature_author sig) $ \c'author -> do 
    withCString (signature_email sig) $ \c'email ->  do
      let c'time = C'git_time (fromIntegral $ time_epoch $ signature_time sig) (fromIntegral $ time_offset $ signature_time sig)
          c'sig = C'git_signature c'author c'email c'time
      action c'sig
      
with_signature_ptr::Signature -> (Ptr C'git_signature -> Result a) -> Result a
with_signature_ptr sig action = with_signature sig $ \c'sig -> with c'sig action
      
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
        with_signature_ptr author $ \c'author_ptr -> do
          with_signature_ptr committer $ \ c'committer_ptr -> do
            withCString message $ \message_ptr -> do
              withForeignPtr (oid_ptr tree) $ \tree_ptr -> do
                withForeignPtrs (map oid_ptr parents) $ \parent_oid_ptrs -> do
                  parent_oids <- mapM peek parent_oid_ptrs
                  withArray parent_oids $ \parent_oid_array -> do
                    c'git_commit_create result_oid_ptr repo_ptr ref_ptr c'author_ptr c'committer_ptr message_ptr tree_ptr (fromIntegral $ length parents) parent_oid_array `handle_git_return` (return $ Oid oid_fptr)