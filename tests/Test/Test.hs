{-# OPTIONS_GHC -XTemplateHaskell #-}
module Test.Test where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.Framework.TH

prop_create_and_lookup_a_commit f = f == True

main = $(defaultMainGenerator)