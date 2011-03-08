module Test.Util where

import Data.Maybe
import Test.QuickCheck
import System.FilePath
import System.Posix.Directory
import System.Directory
import System.IO.Unsafe
import Control.Exception
import qualified Git      

anyString = listOf $ choose('\001', '\255')

shrink3 c a1 a2 a3 = s1 ++ s2 ++ s3
  where
    s1 = [c sa1 a2 a3 | sa1 <- shrink a1 ]
    s2 = [c a1 sa2 a3 | sa2 <- shrink a2 ]
    s3 = [c a1 a2 sa3 | sa3 <- shrink a3 ]

shrink5 c a1 a2 a3 a4 a5 = s1 ++ s2 ++ s3 ++ s4 ++ s5
  where
    s1 = [c sa1 a2 a3 a4 a5 | sa1 <- shrink a1 ]
    s2 = [c a1 sa2 a3 a4 a5 | sa2 <- shrink a2 ]
    s3 = [c a1 a2 sa3 a4 a5 | sa3 <- shrink a3 ]
    s4 = [c a1 a2 a3 sa4 a5 | sa4 <- shrink a4 ]
    s5 = [c a1 a2 a3 a4 sa5 | sa5 <- shrink a5 ]

run title prop = do
  print $ "######### Test: " ++ title
  quickCheckResult prop

inSandboxDirectory::FilePath -> IO a -> IO a
inSandboxDirectory sandboxLocation io = bracket enterSandbox exitSandbox (const io) 
  where
    createParents = True
    enterSandbox = do
      previousWD <- getWorkingDirectory
      createDirectoryIfMissing createParents sandboxLocation
      changeWorkingDirectory sandboxLocation
      return previousWD
    exitSandbox previousWD = do
      changeWorkingDirectory previousWD
      removeDirectoryRecursive sandboxLocation
          
exceptionToFalse::SomeException -> IO Bool
exceptionToFalse _ = return False
        
given_a_repository block =                     
  sandbox $ bracket createRepo freeRepo block
    where
      createRepo = 
        fromJust `fmap` Git.initialize "repository" False
      freeRepo repo = Git.free repo

sandbox::IO a -> a
sandbox io = 
  unsafePerformIO $ 
    inSandboxDirectory "sandbox" io
