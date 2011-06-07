module Git.Blob (
  lookup
  , write
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

write::Repository -> ByteString.ByteString -> IO Oid
write repo contents = withCRepository repo $ \c'repo -> 
  alloca $ \c'oid -> do
    useAsCStringLen contents $ \(c'buffer, size) -> do
      c'git_blob_create_frombuffer c'oid c'repo c'buffer (fromIntegral size) `wrap_git_result` fromCOid c'oid

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
    