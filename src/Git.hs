module Git (Repository
           , Commit(..)
           , GitTime(..)
           , Signature(..)
           , Blob
           , Tree(..)
           , TreeEntry(..)
           , Object(..)
           , Oid
           , oidCpy
           , oidMkStr
           , open
           , initialize
           , free
           , commitLookup
           , commitWrite
           ) where
import Data.List(sort)
import Bindings.Libgit2
import Data.Maybe
import Data.String
import Foreign hiding (free)
import Foreign.C.String
import Foreign.C.Types
import Control.Monad(when, liftM3, forM, forM_)
import Control.Applicative
import Data.ByteString.Char8 hiding (take, length, sort)

data GitTime = GitTime { git_time::Int
                       , git_offset::Int
                       }
             deriving (Show, Eq)
c'git_time_to_haskell (C'git_time time offset) = 
  GitTime { git_time = fromEnum time
          , git_offset = fromEnum offset }  

haskell_to_c'git_time time =
  C'git_time (toEnum $ git_time time) (toEnum $ git_offset time) 
               
data Signature = Signature { signature_name :: String
                           , signature_email :: String
                           , signature_time :: GitTime
                           }
               deriving (Show, Eq)
                        
withCSignature Signature { signature_name, signature_email, signature_time } block = do                        
  withCString signature_name $ \c'name ->
    withCString signature_email $ \c'email -> 
    alloca $ \sigPtr -> do 
      poke sigPtr $ C'git_signature c'name c'email $ haskell_to_c'git_time signature_time
      block sigPtr

data Commit = Commit { commit_id :: Oid
                     , commit_message :: String
                     , commit_short_message :: String
                     , commit_author :: Signature
                     , commit_committer :: Signature
                     , commit_time :: Int
                     , commit_time_offset :: Int
                     , commit_tree :: Oid
                     , commit_parents :: [Oid]
                     }
            deriving (Show, Eq)
              
fromGitCommit::Ptr C'git_commit -> IO Commit
fromGitCommit commitPtr = do
  oid <- c'git_commit_id commitPtr >>= oidCpy . Oid
  msg <- c'git_commit_message commitPtr >>= peekCString
  short_msg <- c'git_commit_message_short commitPtr >>= peekCString
  author <- c'git_commit_author commitPtr >>= peekSignature
  committer <- c'git_commit_committer commitPtr >>= peekSignature
  commit_time <-fromEnum `fmap` c'git_commit_time commitPtr
  time_offset <- fromEnum `fmap` c'git_commit_time_offset commitPtr
  tree <- commitTreeOid
  parents <- parentOids 
  return $ Commit { commit_id = oid
                  , commit_message = msg
                  , commit_short_message = short_msg
                  , commit_author = author
                  , commit_committer = committer
                  , commit_time = commit_time
                  , commit_time_offset = time_offset
                  , commit_tree = tree
                  , commit_parents = parents
                  }                            
  where
    peekSignature sigPtr = peek sigPtr >>= \(C'git_signature name email when) -> 
      liftM3 Signature (peekCString name) (peekCString email) (return $ c'git_time_to_haskell when)  
    parentOids = return []
    commitTreeOid = return $ fromJust $ oidMkStr (take 40 $ repeat 'a')
    
newtype Oid = Oid { oidPtr::Ptr C'git_oid }
instance Ord Oid where
  oid_left `compare` oid_right = oidFmt oid_left `compare` oidFmt oid_right

newtype Repository = Repository { repoPtr :: Ptr C'git_repository }

data TreeEntry = TreeEntry { tree_entry_oid::Oid
                           , tree_entry_filepath::FilePath
                           , tree_entry_attributes::Int } 
               deriving (Eq, Show, Ord)
data Tree = Tree { tree_entries::[TreeEntry] }
            deriving (Show)
                     
instance Eq Tree where
  tree_left == tree_right = (tree_left `compare` tree_right) == EQ
instance Ord Tree where
  tree_left `compare` tree_right = sort (tree_entries tree_left) `compare` sort (tree_entries tree_right)
                     
writeObject optr = do
  r <- c'git_object_write $ castPtr optr
  oidPtr <- c'git_object_id $ castPtr optr
  Just `fmap` oidCpy (Oid oidPtr)
  
instance Object Tree where
  write Repository { repoPtr } Tree { tree_entries } = do
    Just treePtr <- git_out_param $ (\treeOutPtr -> c'git_tree_new treeOutPtr repoPtr)
    forM_ tree_entries $ addTreeEntry treePtr
    writeObject treePtr
    where    
      addTreeEntry treePtr TreeEntry { tree_entry_oid, tree_entry_attributes, tree_entry_filepath} = 
        withCString tree_entry_filepath $ \c'filepath -> do
           r <- c'git_tree_add_entry nullPtr treePtr (oidPtr tree_entry_oid) c'filepath $ toEnum tree_entry_attributes
           when (r < 0) $ error "error adding tree_entry"   

  lookup Repository { repoPtr } Oid { oidPtr } = 
    git_out_param (\treeOutPtr -> c'git_tree_lookup treeOutPtr repoPtr oidPtr) >>= fmapMaybeInF getTree
      where
        getTree treePtr = do
          c'entries <- c'git_tree_entrycount treePtr
          fmap Tree $ forM [0 .. c'entries - 1] $ \idx ->
            c'git_tree_entry_byindex treePtr idx >>= getTreeEntry
        getTreeEntry treeEntryPtr = liftM3 TreeEntry oid filename attributes
          where
            oid = c'git_tree_entry_id treeEntryPtr >>= oidCpy . Oid
            attributes = fmap fromEnum $ c'git_tree_entry_attributes treeEntryPtr  
            filename = c'git_tree_entry_name treeEntryPtr >>= peekCString 
            
            
data Blob = Blob { blob::ByteString }
            deriving (Show, Eq)
                     
instance IsString Blob where 
  fromString = Blob . fromString

instance Show Oid where
  show oid = "{ Oid: " ++ oidFmt oid ++ " }"
instance Eq Oid where
  oid_l == oid_r = oidFmt oid_l == oidFmt oid_r   

oidCpy::Oid -> IO Oid
oidCpy (Oid ptr) = do
  newOidPtr <- malloc
  c'git_oid_cpy newOidPtr ptr 
  return $ Oid newOidPtr

oidFmt::Oid -> String
oidFmt Oid { oidPtr } = unsafePerformIO $ do
  allocaBytes c'GIT_OID_HEXSZ $ \output -> do 
    c'git_oid_fmt output oidPtr
    peekCStringLen (output, c'GIT_OID_HEXSZ)

oidMkStr::String -> Maybe Oid
oidMkStr hex 
  | length hex /= 40 = Nothing
  | otherwise = unsafePerformIO $ do
    withCString hex $ \c'hex -> do
      oidPtr <- malloc
      r <- c'git_oid_mkstr oidPtr c'hex 
      if (r < 0) then return Nothing
        else return $ Just $ Oid oidPtr

open::FilePath -> IO (Maybe Repository)
open path = fmap'2 Repository $ git_out_param go
  where 
    go repoPtr = withCString path $ \pathPtr -> do
      c'git_repository_open repoPtr pathPtr
        
fmap'2 f = fmap (fmap f)

initialize::FilePath -> Bool -> IO (Maybe Repository)
initialize path is_bare = fmap'2 Repository $ git_out_param go
  where 
    go repoPtr = withCString path $ \pathPtr -> do
      c'git_repository_init repoPtr pathPtr $ c'bool is_bare

free::Repository -> IO ()
free = c'git_repository_free . repoPtr

class Object o where
  lookup::Repository -> Oid -> IO (Maybe o)
  write::Repository -> o -> IO (Maybe Oid)

instance Object Blob where
  lookup Repository { repoPtr } Oid { oidPtr } = do
    git_out_param (\blobOut -> c'git_blob_lookup blobOut repoPtr oidPtr) >>= fmapMaybeInF createBlob
      where
        createBlob blobPtr = do
          buffer <- c'git_blob_rawcontent blobPtr
          buffer_size <- c'git_blob_rawsize blobPtr
          Blob `fmap` packCStringLen (buffer, fromEnum buffer_size)
  write Repository { repoPtr } Blob { blob } = do
    Just c'blob <- git_out_param $ (\blobParam -> c'git_blob_new blobParam repoPtr)
    useAsCStringLen blob $ \(blob_array, blob_array_size) -> c'git_blob_set_rawcontent c'blob (castPtr blob_array) $ toEnum blob_array_size
    r <- c'git_object_write $ castPtr c'blob
    oidPtr <- c'git_object_id $ castPtr c'blob                  
    Just `fmap` oidCpy (Oid oidPtr)

commitLookup::Repository -> Oid -> IO (Maybe Commit)
commitLookup Repository { repoPtr } Oid { oidPtr } = 
  git_out_param (\commitPtr -> c'git_commit_lookup commitPtr repoPtr oidPtr) >>= fmapMaybeInF fromGitCommit

fmapMaybeInF::(Monad m, Functor m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeInF g Nothing = return Nothing
fmapMaybeInF g (Just argument) = Just `fmap` g argument
           
  
commitWrite::Repository -> Commit -> IO (Maybe Oid)
commitWrite Repository { repoPtr } Commit { commit_message
                                          , commit_author
                                          , commit_committer
                                          , commit_time
                                          , commit_time_offset
                                          , commit_parents
                                          , commit_tree 
                                          , commit_id } = do
  Just commitPtr <- git_out_param (\commitPtr -> c'git_commit_new commitPtr repoPtr)
  commit_message `withCString` c'git_commit_set_message commitPtr 
  commit_author `withCSignature` c'git_commit_set_author commitPtr
  commit_committer `withCSignature` c'git_commit_set_committer commitPtr
  c'git_object_write $ castPtr commitPtr
  savedOidPtr <- c'git_commit_id commitPtr
  Just `fmap` oidCpy (Oid savedOidPtr)

git_out_param::(Ptr (Ptr a) -> IO CInt) -> IO (Maybe (Ptr a))
git_out_param git_call  = alloca $ \outParam -> do
  r <- git_call outParam
  if (r < 0) then return Nothing
    else Just `fmap` peek outParam

c'bool::Bool -> CInt
c'bool True = 1
c'bool False = 0
           