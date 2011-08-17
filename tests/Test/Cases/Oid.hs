{-# LANGUAGE ScopedTypeVariables #-}
module Test.Cases.Oid where
import qualified Git.Oid as Oid
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.Util

prop_read_and_write_oid oid = Oid.fromstr (Oid.fmt oid) == oid

newtype OidStr = OidStr String
               deriving Show
instance Arbitrary OidStr where
  arbitrary = fmap OidStr $ vectorOf 40 $ elements $ ['a'..'f'] ++ ['0'..'9']

prop_oid_fromstr (OidStr oidstr) = Oid.fmt (Oid.fromstr oidstr) == oidstr  

prop_oid_reflexive (oid::Oid.Oid) = oid == oid

prop_oid_eq (OidStr left) (OidStr right) = (left == right) <=> (Oid.fromstr left == Oid.fromstr right)
  where
    a <=> b = a && b || not a && not b
tests = testGroup "Test.Cases.Oid" [testProperty "read and write oid" prop_read_and_write_oid
                                   , testProperty "fromstr and fmt with" prop_oid_fromstr
                                   , testProperty "oid reflexive" prop_oid_reflexive
                                   , testProperty "oid equality" prop_oid_eq
                                   ]
