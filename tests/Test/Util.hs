module Test.Util where
import Git.Repository
import Git.Result
import Git.Error
import Git.Types.Arbitrary
import Git.Types

import System.IO.Temp
import System.Directory

import Control.Exception
import Prelude hiding (init, catch)

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)


with_repo :: (Testable a) => (Repository -> IO a) -> Property  
with_repo c = morallyDubiousIOProperty $ do
  withSystemTempDirectory "tmp.git" $ \p -> init p >>= c

fails :: IO a -> IO ()
fails c = go  `catch` (\Error {} -> return ())
  where
    go = c >> throwIO (AssertionFailed "Expected Git.Error exception, but dit not get any exception")
  
assertEqual::(Eq a, Show a) => a -> a -> IO Bool
assertEqual expected actual | expected == actual = return True
                            | otherwise = throwIO (AssertionFailed $ "Not Equal. Expected: " 
                                                   ++ show expected 
                                                   ++ " Got: " 
                                                   ++ show actual)

instance Arbitrary Attributes where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = fmap fromIntegral . filter inBounds . shrink . toInteger 
    where 
      inBounds v = toInteger (minBound::Attributes) <= v && v <= toInteger (maxBound::Attributes)