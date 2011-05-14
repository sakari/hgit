module Git.Result where
import Foreign.C.Types
type Result a = IO (Either ErrorCode a)

newtype ErrorCode = ErrorCode Int

handle_git_return::IO CInt -> IO a -> Result a
handle_git_return action wrapper = do
  return_value <- action
  if return_value /= 0 then return $ Left $ ErrorCode $ fromIntegral return_value
    else fmap Right wrapper

