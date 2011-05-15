{-# LANGUAGE ScopedTypeVariables #-}
module Test.Cases.Oid where
import qualified Git.Oid as Oid
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.Util

prop_read_and_write_oid oid = Oid.mkstr (Oid.fmt oid) == oid

newtype OidStr = OidStr String
               deriving Show
instance Arbitrary OidStr where
  arbitrary = fmap OidStr $ vectorOf 40 $ elements $ ['a'..'f'] ++ ['0'..'9']

prop_oid_mkstr (OidStr oidstr) = Oid.fmt (Oid.mkstr oidstr) == oidstr  

prop_oid_reflexive (oid::Oid.Oid) = oid == oid

prop_oid_eq (OidStr left) (OidStr right) = (left == right) <=> (Oid.mkstr left == Oid.mkstr right)
  where
    a <=> b = a && b || not a && not b
tests = testGroup "Test.Cases.Oid" [testProperty "read and write oid" prop_read_and_write_oid
                                   , testProperty "mkstr and fmt with" prop_oid_mkstr
                                   , testProperty "oid reflexive" prop_oid_reflexive
                                   , testProperty "oid equality" prop_oid_eq
                                   ]
