module Git.Types where
import Git.Oid

data TreeEntry = TreeEntry { treeEntryName::EntryName, treeEntryOid::Oid }
                 deriving (Eq, Show)
                          
newtype EntryName = EntryName { entryName::String }
                    deriving (Eq, Show, Ord)
