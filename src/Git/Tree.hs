module Git.Tree where
import Bindings.Libgit2
import Foreign.ForeignPtr
import Foreign.Ptr
import Git.Object
import Git.Repository
import Git.Oid
import Git.Result
import qualified Git.TreeBuilder as Builder

data Tree = Tree { tree_ptr::ForeignPtr C'git_tree }

lookup::Repository -> Oid -> IO Tree
lookup repo oid = lookup_wrapped_object repo oid Tree c'GIT_OBJ_TREE

write::Repository -> [(FilePath, Oid)] -> IO Oid
write repo paths = do
  builder <- Builder.create
  mapM_ (uncurry $ Builder.insert builder) paths
  Builder.write repo builder 
