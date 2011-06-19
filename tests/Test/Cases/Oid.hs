{-# LANGUAGE ScopedTypeVariables #-}
module Test.Cases.Oid where
import qualified Git.Oid as Oid
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Git.Types.Arbitrary()
newtype OidStr = OidStr String
               deriving Show
instance Arbitrary OidStr where
  arbitrary = fmap OidStr $ vectorOf 40 $ elements $ ['a'..'f'] ++ ['0'..'9']

tests::Test
tests = testGroup "Test.Cases.Oid" [testProperty "read and write oid" $ \oid -> 
                                     Oid.mkstr (Oid.fmt oid) == oid
                                     
                                   , testProperty "mkstr and fmt with" $ \(OidStr oidstr) -> 
                                     Oid.fmt (Oid.mkstr oidstr) == oidstr  
                                     
                                   , testProperty "oid reflexive" $ \(oid::Oid.Oid) ->
                                     oid == oid
                                     
                                   , testProperty "oid equality" $ \(OidStr left) (OidStr right) ->
                                     let a <=> b = a && b || not a && not b
                                     in (left == right) <=> (Oid.mkstr left == Oid.mkstr right)
                                   ]
