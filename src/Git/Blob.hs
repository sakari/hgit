module Git.Blob (
  lookup
  ) where

import Bindings.Libgit2
import Git.Oid
import Git.Repository
import Prelude hiding (lookup)
import Data.ByteString as ByteString
import Foreign.Marshal.Alloc
import Git.Result
import Foreign.Storable
import Foreign.Ptr

lookup::Repository -> Oid -> IO ByteString.ByteString
lookup repo oid = withCRepository repo $ \c'repo -> 
  withCOid oid $ \c'oid -> do
    alloca $ \p'blob ->  
      c'git_blob_lookup p'blob c'repo c'oid `wrap_git_result` (peek p'blob >>= go)
        where
          go c'blob = do
            sz <- c'git_blob_rawsize c'blob
            p'content <- c'git_blob_rawcontent c'blob
            r <- ByteString.packCStringLen (castPtr p'content, fromIntegral sz) 
            c'git_blob_close c'blob
            return r
    