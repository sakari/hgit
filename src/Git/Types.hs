module Git.Types where
import Git.Oid

data TreeEntry = TreeEntry { treeEntryName::EntryName, treeEntryOid::Oid }
newtype EntryName = EntryName { entryName::String }
                    deriving (Eq, Show, Ord)
