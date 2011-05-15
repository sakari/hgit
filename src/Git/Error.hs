module Git.Error where
import Bindings.Libgit2
import Foreign.C.Types
import Foreign.C.String

lastError::IO String
lastError = c'git_lasterror >>= peekCString
