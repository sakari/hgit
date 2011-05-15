module Test.Util where
import Git.Repository
import Git.Result
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)
import System.IO.Temp
import System.Directory
import Control.Exception
import Prelude hiding (init)

with_repo :: (Testable a) => (Repository -> IO a) -> Property  
with_repo c = morallyDubiousIOProperty $ do
  withSystemTempDirectory "tmp.git" $ \path -> do
    r <- init path
    case r of 
      Left i -> error $ "got code: " ++ show i
      Right repo -> c repo

fails :: Result a -> IO ()
fails c = do
  Left _ <- c
  return ()
  
  
  