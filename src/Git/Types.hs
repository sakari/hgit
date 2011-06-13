{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types shared by modules 

module Git.Types(
  EntryName
  , entryToPath
  , pathToEntry
  , unsafePathToEntry
  , TreeEntry(..)
  , Attributes(..)
                ) where
import Git.Oid
import Data.Maybe
import System.FilePath
import Data.List

data TreeEntry = TreeEntry { treeEntryName::EntryName, treeEntryOid::Oid, treeEntryAttributes::Attributes }
                 deriving (Eq, Show, Ord)
                          
-- | A name for an entry relative to the repository root
data EntryName = EntryName { entryName:: !String }
                    deriving (Eq, Show, Ord)
                             
-- | Get the underlying path for the 'EntryName'

entryToPath::EntryName -> FilePath
entryToPath = entryName

-- | Convert a path to an 'EntryName'
-- The path needs to be relative:
--
-- >>> pathToEntry "/foo"
-- Nothing
--
-- And not point anywhere else than the repository:
--
-- >>> pathToEntry "../foo"
-- Nothing
-- 
-- The entry name will be canonicalized in the process
-- 
-- >>> entryToPath `fmap` pathToEntry "a/../b" 
-- Just "b"

pathToEntry::FilePath -> Maybe EntryName
pathToEntry path 
  | isRelative norm && isValid norm && not (".." `isPrefixOf` contracted) = 
    Just $ EntryName contracted
  | otherwise = Nothing
  where
    norm = normalise path
    contracted = contract norm
    
-- | Contract a path:
--
-- >>> contract "a"
-- "a"
-- >>> contract "a/.."
-- ""
-- >>> contract "a/."
-- "a"
-- >>> contract "a/../b"
-- "b"
-- >>> contract "a/../.."
-- ".."
contract::FilePath -> FilePath
contract p = joinPath $ unstack $ foldr go (0, []) $ splitDirectories p
  where
    unstack (stack, accum) = replicate stack ".." ++ accum
    go ".." (stack, accum) = (stack + 1, accum)
    go "." a = a
    go a (stack, accum) | stack > 0 = (stack - 1, accum)
    go a (stack, accum) = (stack, a:accum)

-- | Unsafe version of 'pathToEntry'
--    
-- Will 'error' when 'pathToEntry' would produce 'Nothing'
--    
-- >>> unsafePathToEntry "/foo"
-- *** Exception: Illegal path for EntryName: /foo

unsafePathToEntry::FilePath -> EntryName
unsafePathToEntry path = (error $ "Illegal path for EntryName: " ++ path ) `fromMaybe` pathToEntry path

newtype Attributes = Attributes { attributes::Int }
                     deriving (Eq, Show, Ord, Num, Integral, Real, Enum)

instance Bounded Attributes where
  maxBound = 0o777777
  minBound = 0