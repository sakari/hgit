module Main where
import Test.Framework
import Test.Framework.DocTest
import System.IO.Temp
import System.Directory

main = withTempDirectory "." "doctest" $ \tmp -> do
  setCurrentDirectory tmp
  docTest  ["../src/Git.hs"] ["-i../src"] >>= defaultMain . return
  