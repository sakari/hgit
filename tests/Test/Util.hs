module Test.Util where
import Git.Repository
import Git.Result
import Git.Oid
import Git.Commit
import Git.Error

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)

import System.IO.Temp
import System.Directory

import Control.Exception
import Control.Applicative

import Prelude hiding (init)

instance Arbitrary Oid where
  arbitrary = mkstr `fmap` vectorOf 40 arbitrary
  
instance Arbitrary TimeOffset where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink (TimeOffset t) = filter inBounds $ map TimeOffset $ shrink t 
    where
      inBounds t = minBound <= t && t <= maxBound 
      
instance Arbitrary Signature where
  arbitrary = Signature <$> arbitraryString <*> arbitraryString <*> arbitrary
    where
      arbitraryString = listOf1 $ elements ['a'..'z']
  shrink (Signature author committer time) = Signature author committer `map` shrink time

instance Arbitrary Epoch where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink (Epoch e) = filter inBounds $ map Epoch $ shrink e
    where
      inBounds t = minBound <= t && t <= maxBound

instance Arbitrary Time where  
  arbitrary = Time <$> arbitrary <*> arbitrary
  shrink (Time epoch offset) = (Time <$> pure epoch <*> shrink offset) ++ (Time <$> shrink epoch <*> pure offset)

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
    go (Left i) = do
      err <- lastError
      error $ "got code: " ++ show i ++ " " ++ err 
    go (Right r) = return r
  
  