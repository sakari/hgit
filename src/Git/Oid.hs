module Git.Oid where
import Bindings.Libgit2
import Foreign.ForeignPtr
import System.IO.Unsafe
import Foreign.C.String

data Oid = Oid { oid_ptr::ForeignPtr C'git_oid }

instance Eq Oid where
  left == right = fmt left == fmt right

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