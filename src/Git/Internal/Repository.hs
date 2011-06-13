{-# LANGUAGE NamedFieldPuns #-}

module Git.Internal.Repository (
   WithAnyRepository(..)
   , withCRepository
   , withCBareRepository
   , withCFRepository
   , withCFBareRepository
   , fromCRepository
   , fromCAnyRepository
   , fromCBareRepository
   , Repository
   , BareRepository
   , AnyRepository     
   )
       where

import Bindings.Libgit2
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

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
    
fromCR::(ForeignPtr C'git_repository -> a) -> Ptr C'git_repository -> IO a
fromCR con c'repo = fmap con $ newForeignPtr c'repo $ c'git_repository_free c'repo

fromCAnyRepository::Ptr C'git_repository -> IO AnyRepository  
fromCAnyRepository = fromCR AnyRepository
  
fromCBareRepository::Ptr C'git_repository -> IO BareRepository  
fromCBareRepository = fromCR BareRepository

fromCRepository::Ptr C'git_repository -> IO Repository  
fromCRepository = fromCR Repository
