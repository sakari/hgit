module Main where
import qualified Test.Cases
import Test.Framework

main :: IO ()
main = defaultMain Test.Cases.tests
