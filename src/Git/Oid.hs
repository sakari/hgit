module Git.Oid where
import Bindings.Libgit2
import Foreign.ForeignPtr

data Oid = Oid { oid_ptr::ForeignPtr C'git_oid }
