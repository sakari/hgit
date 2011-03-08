module Test.Arbitrary where

import Test.Util
import Test.QuickCheck
import Control.Monad
import qualified Git
import Data.Maybe
import Data.String

newtype RepoName = RepoName FilePath 
                 deriving Show
                          
instance Arbitrary RepoName where
  arbitrary = RepoName `fmap` listOf1 (choose ('a', 'z'))
  
instance Arbitrary Git.Oid where  
  arbitrary = (fromJust . Git.oidMkStr) `fmap` vectorOf 40 (oneof [choose ('a', 'f'), choose('0', '9')])
  
instance Arbitrary Git.Tree where
  arbitrary = liftM Git.Tree $ listOf1 arbitrary
  shrink = fmap Git.Tree . shrink . Git.tree_entries 

newtype Path = Path { fromPath::FilePath }
instance Arbitrary Path where
  arbitrary = fmap Path $ listOf validPathChars
    where
      -- this is certainly too narrow set
      validPathChars = choose('a', 'z') 
  shrink = fmap Path . shrink . fromPath
  
  
newtype FileMode = FileMode { fromFileMode::Int }
                 deriving (Eq, Ord, Show, Num)  
                          
instance Arbitrary FileMode where
  arbitrary = fmap FileMode $ choose(0, 0o777777)
  shrink n | n > 0 = [n - 1]
           | otherwise = [] 
    
instance Arbitrary Git.TreeEntry where
  arbitrary = liftM3 Git.TreeEntry arbitrary (fmap fromPath arbitrary) (fmap fromFileMode arbitrary)
  shrink Git.TreeEntry { Git.tree_entry_oid, Git.tree_entry_filepath, Git.tree_entry_attributes } = 
    shrink3 Git.TreeEntry tree_entry_oid tree_entry_filepath tree_entry_attributes
    
instance Arbitrary Git.Blob where
  arbitrary = fromString `fmap` listOf1 arbitrary
  
instance Arbitrary Git.GitTime where  
  arbitrary = liftM2 Git.GitTime arbitrary arbitrary
        
instance Arbitrary Git.Signature where  
  arbitrary = liftM3 Git.Signature anyString anyString arbitrary
  shrink Git.Signature { Git.signature_name, Git.signature_email, Git.signature_time } = 
    shrink3 Git.Signature signature_name signature_email signature_time 
    
instance Arbitrary Git.Commit where
  arbitrary = liftM5 Git.Commit anyString arbitrary arbitrary arbitrary $ return []
  shrink (Git.Commit a b c d e) = shrink5 Git.Commit a b c d e
  
