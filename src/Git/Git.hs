module Git.Git where
type Result a = IO (Either Int a)

handle_git_return::IO Int -> IO a -> Result a
handle_git_return action wrapper = do
  return_value <- action
  if return_value != 0 then return $ Left return_value
    else wrapper

