module Main where
import Test.Framework
import Test.Framework.DocTest
import System.IO.Temp
import System.Directory
import Control.Exception

main::IO ()
main = withTempDirectory "." "doctest" $ \tmp -> do
  cwd <- getCurrentDirectory
  setCurrentDirectory tmp
  tests <- docTest  ["../src/Git.hs"] ["-i../src"]
  defaultMain [tests] `finally` setCurrentDirectory cwd