module Test.Util where
import Git.Repository
import Git.Result
import Git.Oid
import Git.Commit

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)

import System.IO.Temp
import System.Directory

import Control.Exception
import Control.Applicative

import Prelude hiding (init)

instance Arbitrary Oid where
  arbitrary = mkstr `fmap` vectorOf 40 arbitrary
  
instance Arbitrary Signature where
  arbitrary = Signature <$> arbitraryString <*> arbitraryString <*> arbitrary
    where
      arbitraryString = listOf1 $ elements ['a'..'z']

instance Arbitrary Time where  
  arbitrary = Time <$> arbitrary <*> arbitrary


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
  
success::Result a -> IO a 
success = (go =<<) 
  where
    go (Left i) = error $ "got code: " ++ show i 
    go (Right r) = return r
  
  