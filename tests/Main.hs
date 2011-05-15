module Main where
import System.FilePath.Glob
import System.Process
import System.Exit

main = do
  files <- compile "Test/**/*.hs" `globDir1` "tests"
  r <- mapM go files
  if any gotErrorCode r 
    then exitFailure
    else exitSuccess 
  where
    gotErrorCode ExitSuccess = False
    gotErrorCode _ = True
    go path = do
      (exitValue, stdout, stderr) <- readProcessWithExitCode "runghc" [path] ""
      putStrLn $ "code: " ++ show exitValue ++ stdout ++ stderr
      return exitValue