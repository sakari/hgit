module Git.Result (wrap_git_result) where

import Git.Error
import Bindings.Libgit2
import Foreign.C.Types
import Foreign.C.String
import Control.Exception

lastError::IO String
lastError = c'git_lasterror >>= peekCString

wrap_git_result::IO CInt -> IO a -> IO a
wrap_git_result action wrapper = do
  return_value <- action
  if return_value == 0 then wrapper 
    else do
      explanation <- lastError
      throwIO $ fromIntegral return_value `Error` explanation 

