{-# LANGUAGE NamedFieldPuns #-}

{-|
 There are three kinds of types for repositories 'Repository', 'BareRepository' and 'AnyRepository' when you do not care whether 
 the repository is bare or not. 
-}

module Git.Repository (init
                      , initBare
                      , open
                      , openAny
                      , Repository
                      , BareRepository
                      , AnyRepository
                      , WithAnyRepository
                      , withCAnyRepository
                      , withCFAnyRepository
                      , anyRepository
                      , writeFile
                      , withCRepository
                      , withCBareRepository
                      , withCFRepository
                      , withCFBareRepository
                      , workdir) where
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Bindings.Libgit2
import Control.Monad
import Git.Result
import Git.Types
import qualified Data.ByteString as ByteString
import Prelude hiding (init, writeFile)
import qualified Prelude
import System.FilePath
import System.Directory
import Data.Char

data Repository = Repository { repository_ptr::ForeignPtr C'git_repository }
data BareRepository = BareRepository { bare_repository_ptr::ForeignPtr C'git_repository }
data AnyRepository = AnyRepository { any_repository_ptr::ForeignPtr C'git_repository }

class WithAnyRepository repo where
  withCFAnyRepository::repo -> (ForeignPtr C'git_repository -> IO a) -> IO a
  withCAnyRepository::repo -> (Ptr C'git_repository -> IO a) -> IO a
  anyRepository::repo -> AnyRepository
  
  withCAnyRepository repo c = withCFAnyRepository repo $ \f'repo -> 
    withForeignPtr f'repo c

instance WithAnyRepository AnyRepository where
  withCFAnyRepository AnyRepository { any_repository_ptr } c = c any_repository_ptr
  anyRepository = id
  
instance WithAnyRepository Repository where
  withCFAnyRepository = withCFRepository
  anyRepository = AnyRepository . repository_ptr

instance WithAnyRepository BareRepository where
  withCFAnyRepository = withCFBareRepository
  anyRepository = AnyRepository . bare_repository_ptr

withCFBareRepository::BareRepository -> (ForeignPtr C'git_repository -> IO a) -> IO a
withCFBareRepository BareRepository { bare_repository_ptr } c = c bare_repository_ptr

withCBareRepository::BareRepository -> (Ptr C'git_repository -> IO a) -> IO a
withCBareRepository repo c = withCFBareRepository repo $ \f'repo ->
  withForeignPtr f'repo c

withCFRepository::Repository -> (ForeignPtr C'git_repository -> IO a) -> IO a
withCFRepository Repository { repository_ptr } c = c repository_ptr

withCRepository::Repository -> (Ptr C'git_repository -> IO a) -> IO a
withCRepository repo c = withCFRepository repo $ \f'repo ->
  withForeignPtr f'repo c
  
newRepository::FilePath -> (ForeignPtr C'git_repository -> IO c) -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt) -> IO c
newRepository path con constructor = alloca $ \ptr_ptr -> withCString path $ \c'path ->
  constructor ptr_ptr c'path `wrap_git_result` wrap ptr_ptr
    where
      wrap ptr_ptr = do
        repo_ptr <- peek ptr_ptr
        repo_ptr `newForeignPtr` c'git_repository_free repo_ptr >>= con

-- | Initialize a new non-bare repository at given path
--
-- >>> init "init-repo"
-- >>> getDirectoryContents "init-repo" >>= putStr . unlines
-- .
-- .git
-- ..

init::FilePath -> IO Repository
init path = newRepository path (return . Repository) $ \pptr c'path -> c'git_repository_init pptr c'path 0
        
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
initBare path = newRepository path (return . BareRepository) $ \p'ptr c'path -> c'git_repository_init p'ptr c'path 1

class Repo a where
  bare::ForeignPtr C'git_repository -> Maybe a
  repo::ForeignPtr C'git_repository -> Maybe a

instance Repo BareRepository where
  bare = Just . BareRepository 
  repo = const Nothing

instance Repo Repository where
  bare = const Nothing
  repo = Just . Repository
  
instance Repo AnyRepository where  
  bare = Just . AnyRepository
  repo = Just . AnyRepository
  
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
open path = newRepository path go c'git_repository_open
  where
    go f'repo = do
      withForeignPtr f'repo $ \c'repo -> do
        r <- (nullPtr == ) `fmap` c'git_repository_workdir c'repo
        return $ if r then bare f'repo 
                 else repo f'repo
      
openAny::FilePath -> IO AnyRepository
openAny path = newRepository path (return . AnyRepository) c'git_repository_open

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
