module Git.Tree where
import Bindings.Libgit2
import Foreign.ForeignPtr
import Foreign.Ptr
import Git.Object
import Git.Repository
import Git.Oid
import Git.Result

data Tree = Tree { tree_ptr::ForeignPtr C'git_tree }

lookup::Repository -> Oid -> Result Tree
lookup repo oid = lookup_wrapped_object repo oid Tree c'GIT_OBJ_TREE
  
