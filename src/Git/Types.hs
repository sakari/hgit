{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Git.Types where
import Git.Oid

data TreeEntry = TreeEntry { treeEntryName::EntryName, treeEntryOid::Oid, treeEntryAttributes::Attributes }
                 deriving (Eq, Show, Ord)
                          
newtype EntryName = EntryName { entryName::String }
                    deriving (Eq, Show, Ord)
                             
newtype Attributes = Attributes { attributes::Int }
                     deriving (Eq, Show, Ord, Num, Integral, Real, Enum)

instance Bounded Attributes where
  maxBound = 0o77777 -- 0o777777
  minBound = 0