{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances #-}

module Git.Types.Arbitrary where
import Control.Applicative

import Git.Oid
import Git.Commit
import Git.Tree
import Git.Types
import qualified Git.Index as Index

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property hiding (Result)
import Data.Char
import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import System.Posix.Types

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
  arbitrary = unsafePathToEntry <$> listOf1 (elements $ validEntryChars)
  shrink name = unsafePathToEntry <$> filter (not . null) (filter isValidEntryChar `map` shrink (entryToPath name)) 
      
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

instance Arbitrary Index.Entry where
  arbitrary = Index.Entry <$> arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary
  shrink (Index.Entry entry_ctime entry_mtime entry_dev entry_ino 
                     entry_mode entry_uid entry_gid entry_file_size
                     entry_oid entry_path) = safe'tail $ Index.Entry 
                                      <$> alts entry_ctime 
                                      <*> alts entry_mtime 
                                      <*> alts entry_dev
                                      <*> alts entry_ino
                                      <*> alts entry_mode
                                      <*> alts entry_uid
                                      <*> alts entry_gid
                                      <*> alts entry_file_size
                                      <*> alts entry_oid
                                      <*> alts entry_path
                                        where
                                          alts e = e : shrink e
                                          safe'tail [] = [] 
                                          safe'tail (_:as) = as
instance Arbitrary Index.Time where
  arbitrary = Index.Time <$> arbitrary <*> arbitrary

instance Arbitrary EpochTime where
  arbitrary = fmap fromInteger arbitrary

instance Arbitrary ByteString.ByteString where
  arbitrary = ByteString.pack `fmap` listOf1 arbitrary

validEntryChars::[Char]
validEntryChars = filter valid $ map chr [0 .. 256]
  where
    valid c = isAlphaNum c && isAscii c

validEntryCharSet::Set.Set Char
validEntryCharSet = Set.fromList validEntryChars

isValidEntryChar::Char -> Bool
isValidEntryChar = flip Set.member validEntryCharSet
