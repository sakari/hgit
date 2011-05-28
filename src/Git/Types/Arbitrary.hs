{-# LANGUAGE NamedFieldPuns #-}

module Git.Types.Arbitrary where
import Control.Applicative

import Git.Oid
import Git.Commit
import Git.Tree
import Git.Types

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)
import Data.Char
import qualified Data.Set as Set

instance Arbitrary Oid where
  arbitrary = fmap mkstr $ vectorOf 40 $ elements $ ['a' .. 'f'] ++ ['A' .. 'F'] ++ ['0' .. '9']
  shrink oid | fmt oid == replicate 40 '0' = []
             | otherwise = fmap mkstr $ (pad . filter isHexDigit) `map` shrink (fmt oid)   
         where
            pad str = replicate (40 - length str) '0' ++ take 40 str

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

instance Arbitrary EntryName where
  arbitrary = EntryName <$> listOf1 (elements $ validEntryChars)
  shrink EntryName { entryName } = EntryName <$> filter (not . null) (filter isValidEntryChar `map` shrink entryName) 
      
instance Arbitrary TreeEntry where 
  arbitrary = TreeEntry <$> arbitrary <*> arbitrary <*> arbitrary
  shrink TreeEntry { treeEntryName, treeEntryOid, treeEntryAttributes } = 
    (TreeEntry <$> shrink treeEntryName <*> pure treeEntryOid <*> pure treeEntryAttributes) ++
    (TreeEntry <$> pure treeEntryName <*> shrink treeEntryOid <*> pure treeEntryAttributes) ++
    (TreeEntry <$> pure treeEntryName <*> pure treeEntryOid <*> shrink treeEntryAttributes)

instance Arbitrary Attributes where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = fmap fromIntegral . filter inBounds . shrink . toInteger 
    where 
      inBounds v = toInteger (minBound::Attributes) <= v && v <= toInteger (maxBound::Attributes)

validEntryChars::[Char]
validEntryChars = filter valid $ map chr [0 .. 256]
  where
    valid c = isAlphaNum c && isAscii c

validEntryCharSet::Set.Set Char
validEntryCharSet = Set.fromList validEntryChars

isValidEntryChar::Char -> Bool
isValidEntryChar = flip Set.member validEntryCharSet
