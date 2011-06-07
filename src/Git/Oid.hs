module Git.Oid (Oid, fmt, mkstr, withCOid, fromCOid, fromCOidStruct, withCOids) where
import Bindings.Libgit2
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.IO.Unsafe
import Foreign.C.String

data Oid = Oid { oid_ptr::ForeignPtr C'git_oid }

instance Eq Oid where
  left == right = fmt left == fmt right

instance Ord Oid where
  left `compare` right = fmt left `compare` fmt right

instance Show Oid where
  show oid = "Oid " ++ fmt oid

fmt :: Oid -> String
fmt (Oid fptr) = unsafePerformIO $ do
  withForeignPtr fptr $ \ptr -> do
    withCString (replicate 40 ' ') $ \c'string -> do
      c'git_oid_fmt c'string ptr
      peekCString c'string

mkstr :: String -> Oid
mkstr string = unsafePerformIO $ do
  fptr <- mallocForeignPtr 
  withForeignPtr fptr $ \ptr -> 
    withCString string $ \c'string ->
    c'git_oid_mkstr ptr c'string
  return $ Oid fptr 

withCOid::Oid -> (Ptr C'git_oid -> IO a) -> IO a
withCOid (Oid oid_ptr) c = withForeignPtr oid_ptr $ \ptr -> 
  alloca $ \tmp -> peek ptr >>= poke tmp >> c tmp

withCOids::[Oid] -> ([Ptr C'git_oid] -> IO a) -> IO a
withCOids oids f = go oids [] 
  where
    go [] cs = f $ reverse cs
    go (o:os) cs = withCOid o $ \c'oid -> go os (c'oid:cs)

fromCOidStruct::C'git_oid -> IO Oid
fromCOidStruct c'oid = do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr -> poke ptr c'oid
  return $ Oid fptr

fromCOid::Ptr C'git_oid -> IO Oid
fromCOid c'oid = do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr -> peek c'oid >>= poke ptr
  return $ Oid fptr
  
