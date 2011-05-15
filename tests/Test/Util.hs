module Test.Util where
import Git.Repository
import Git.Result
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)

with_repo :: (Testable a) => (Repository -> IO a) -> Property  
with_repo c = morallyDubiousIOProperty $ c $ error "TBD"

fails :: Result a -> IO ()
fails c = do
  Left _ <- c
  return ()
  
  
  