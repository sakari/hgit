module Git.Tree where
import Bindings.Libgit2
import Foreign.ForeignPtr

data Tree = Tree { tree_ptr::ForeignPtr C'git_tree }
