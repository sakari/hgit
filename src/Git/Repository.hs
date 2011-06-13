{-# LANGUAGE NamedFieldPuns #-}

{-|
 There are three kinds of types for repositories 'Repository', 'BareRepository' and 'AnyRepository' when you do not care whether 
 the repository is bare or not. 
-}

module Git.Repository (init
                      , initBare
                      , open
                      , openAny
                      , anyRepository
                      , writeFile
                      , workdir
                      , Repository
                      , BareRepository
                      , AnyRepository
                      ) where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Bindings.Libgit2
import Control.Monad
import Git.Internal.Result
import Git.Internal.Repository
import Git.Types
import qualified Data.ByteString as ByteString
import Prelude hiding (init, writeFile)
import qualified Prelude
import System.FilePath
import System.Directory
import Data.Char

-- | Initialize a new non-bare repository at given path
--
-- >>> init "init-repo"
-- >>> getDirectoryContents "init-repo" >>= putStr . unlines
-- .
-- .git
-- ..

init::FilePath -> IO Repository
init path = alloca $ \c'repo -> withCString path $ \c'path -> do
  c'git_repository_init c'repo c'path 0 `wrap_git_result` peek c'repo >>= fromCRepository
        
-- | Initialize a bare repository at given path
-- 
-- >>> initBare "init-bare"
-- >>> getDirectoryContents "init-bare" >>= putStr . unlines
-- .
-- refs
-- objects
-- ..
-- HEAD

initBare::FilePath -> IO BareRepository
initBare path = alloca $ \c'repo -> withCString path $ \c'path -> do
  c'git_repository_init c'repo c'path 1 `wrap_git_result` peek c'repo >>= fromCBareRepository

class Repo a where
  bare::Ptr C'git_repository -> IO (Maybe a)
  repo::Ptr C'git_repository -> IO (Maybe a)

instance Repo BareRepository where
  bare = fmap Just . fromCBareRepository
  repo = const $ return Nothing

instance Repo Repository where
  bare = const $ return Nothing
  repo = fmap Just . fromCRepository
  
instance Repo AnyRepository where  
  bare = fmap Just . fromCAnyRepository
  repo = fmap Just . fromCAnyRepository
  
-- | Open an existing repository
--  
-- >>> init "open-repo"
-- >>> Just repo <- open "open-repo/.git"::IO (Maybe Repository)
--
-- >>> initBare "open-bare-repo"
-- >>> Just bare <- open "open-bare-repo"::IO (Maybe BareRepository)
--
-- >>> init "open-any-repo"  
-- >>> Just repo <- open "open-any-repo/.git"::IO (Maybe AnyRepository)
-- >>> initBare "open-any-bare-repo"
-- >>> Just bare <- open "open-any-bare-repo"::IO (Maybe AnyRepository)

open::(Repo repo, WithAnyRepository repo) => FilePath -> IO (Maybe repo)
open path = alloca $ \c'repo -> do
  withCString path $ \c'path -> do
    c'git_repository_open c'repo c'path `wrap_git_result` peek c'repo >>= go 
  where
    go c'repo = do
      r <- (nullPtr == ) `fmap` c'git_repository_workdir c'repo
      if r then bare c'repo 
           else repo c'repo
      
openAny::FilePath -> IO AnyRepository
openAny path = alloca $ \c'repo -> do
  withCString path $ \c'path -> do
    c'git_repository_open c'repo c'path `wrap_git_result` peek c'repo >>= fromCAnyRepository 

-- | Get the path to the workdir for the repository

workdir::Repository -> IO FilePath
workdir repo = withCRepository repo $ \c'repo -> 
  c'git_repository_workdir c'repo >>= (fmap Prelude.init . peekCString)

-- | Write 'ByteString' to repository 'workdir'
-- The following holds between 'workdir' and 'writeFile'
-- 
-- >>> repo <- init "writeFile-repo"
-- >>> writeFile repo (unsafePathToEntry "bar") ByteString.empty
-- >>> wd <- workdir repo
-- >>> elem "bar" `fmap` getDirectoryContents wd
-- True

writeFile::Repository -> EntryName -> ByteString.ByteString -> IO ()
writeFile repo name contents = do
  repoPath <- workdir repo
  let absPath = repoPath </> entryToPath name
  absPath `ByteString.writeFile` contents 
