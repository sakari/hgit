module Git.Blob (
  lookup
  , write
  ) where

import Bindings.Libgit2
import Git.Internal.Oid
import Git.Internal.Repository
import Prelude hiding (lookup)
import Data.ByteString as ByteString
import Foreign.Marshal.Alloc
import Git.Internal.Result
import Foreign.Storable
import Foreign.Ptr

write::WithAnyRepository repo => repo -> ByteString.ByteString -> IO Oid
write repo contents = withCAnyRepository repo $ \c'repo -> 
  alloca $ \c'oid -> do
    useAsCStringLen contents $ \(c'buffer, size) -> do
      c'git_blob_create_frombuffer c'oid c'repo c'buffer (fromIntegral size) `wrap_git_result` fromCOid c'oid

lookup::WithAnyRepository repo => repo -> Oid -> IO ByteString.ByteString
lookup repo oid = withCAnyRepository repo $ \c'repo -> 
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
    