{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Git.Commit where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String

import Control.Monad
import Data.Word

import Git.Result
import Git.Repository
import Git.Oid
import Git.Tree
import Git.Object
import System.IO.Unsafe
import Bindings.Libgit2

data Commit = Commit { commit_ptr::ForeignPtr C'git_commit 
                     , commit_repo_ptr::ForeignPtr C'git_repository
                     }

lookup::Repository -> Oid -> IO Commit
lookup repo oid = lookup_wrapped_object repo oid wrap c'GIT_OBJ_COMMIT
  where
    wrap fptr = Commit { commit_ptr = fptr, commit_repo_ptr = repository_ptr repo}
  
type Ref = String
type Message = String
type OidT a = Oid
type Author = Signature
type Committer = Signature

newtype TimeOffset = TimeOffset { timeOffset::Int }
                   deriving (Show, Num, Eq, Integral, Real, Enum, Ord)

instance Bounded TimeOffset where
  minBound = -maxBound
  maxBound = 14 * 60 + 59

newtype Epoch = Epoch { epoch::Int }
                deriving (Show, Num, Eq, Integral, Real, Enum, Ord)
                         
instance Bounded Epoch where
  minBound = 0
  maxBound = Epoch (maxBound::Int)
    
data Time = Time { time_epoch::Epoch, time_offset::TimeOffset }
          deriving (Show, Eq)
                   
data Signature = Signature { signature_author::String, signature_email::String, signature_time::Time }
               deriving (Show, Eq)

with_signature::Signature -> (C'git_signature -> IO a) -> IO a
with_signature sig action = do 
  withCString (signature_author sig) $ \c'author -> do 
    withCString (signature_email sig) $ \c'email ->  do
      let c'time = C'git_time (fromIntegral $ epoch $ time_epoch $ signature_time sig) (fromIntegral $ timeOffset $ time_offset $ signature_time sig)
          c'sig = C'git_signature c'author c'email c'time
      action c'sig
      
with_signature_ptr::Signature -> (Ptr C'git_signature -> IO a) -> IO a
with_signature_ptr sig action = with_signature sig $ \c'sig -> with c'sig action
      
withMaybeCString::Maybe String -> (CString -> IO a) -> IO a
withMaybeCString string action = maybe (action nullPtr) (flip withCString action) string

withForeignPtrs::[ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs action = go fptrs [] 
  where
    go [] fptrs = action $ reverse fptrs
    go (a:as) fptrs = withForeignPtr a $ \fptr -> go as (fptr:fptrs) 

create::Repository -> Maybe Ref -> Author -> Committer -> Message -> OidT Tree -> [OidT Commit] -> IO (OidT Commit) 
create repo ref author committer message tree parents = do  
  alloca $ \result_oid_ptr -> do
    withForeignPtr (repository_ptr repo) $ \repo_ptr -> do
      withMaybeCString ref $ \ref_ptr -> do
        with_signature_ptr author $ \c'author_ptr -> do
          with_signature_ptr committer $ \ c'committer_ptr -> do
            withCString message $ \message_ptr -> do
              withCOid tree $ \tree_ptr -> do
                withCOids parents $ \parent_oid_ptrs -> do
                  withArray parent_oid_ptrs $ \parent_oid_array -> do
                    c'git_commit_create result_oid_ptr repo_ptr ref_ptr c'author_ptr c'committer_ptr message_ptr tree_ptr (fromIntegral $ length parents) parent_oid_array `wrap_git_result` fromCOid result_oid_ptr

fromCTime::C'git_time -> Time
fromCTime (C'git_time stamp offset) = Time (fromIntegral $ fromEnum stamp) (fromIntegral offset)

signature::(Ptr C'git_commit -> IO (Ptr C'git_signature)) -> Commit -> Signature
signature accessor Commit { commit_ptr } = unsafePerformIO $ withForeignPtr commit_ptr $ \ptr -> do
  (C'git_signature name email time) <- accessor ptr >>= peek
  liftM3 Signature (peekCString name) (peekCString email) (return $ fromCTime time)

committer::Commit -> Signature
committer = signature c'git_commit_committer 

author::Commit -> Signature
author = signature c'git_commit_author

tree::Commit -> Oid
tree Commit { commit_ptr } = unsafePerformIO $ do 
  withForeignPtr commit_ptr $ \ptr -> c'git_commit_tree_oid ptr >>= fromCOid

parents::Commit -> [Oid]
parents Commit { commit_ptr } = unsafePerformIO $ do
  withForeignPtr commit_ptr $ \ptr -> do
    parentCount <- c'git_commit_parentcount ptr
    if parentCount <= 0 then return []
      else mapM (go ptr) [0 .. parentCount - 1] 
  where
    go cptr index = c'git_commit_parent_oid cptr index >>= fromCOid
