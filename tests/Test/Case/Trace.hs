module Test.Case.Trace where

import qualified Git
import Control.Monad
import Test.Util
import Test.Arbitrary
import qualified Data.Map as Map
import Test.QuickCheck

data GitAction = GitWriteBlob Git.Blob
               | GitLookupBlob Git.Oid
                 deriving (Show, Eq)
                          
data GitTrace = GitTraceBlob { gitTraceBlob::Maybe Git.Blob}
              | GitTraceOid { gitTraceOid::Maybe Git.Oid}
                deriving (Show, Eq)
                
instance Arbitrary GitAction where
  arbitrary = oneof [fmap GitWriteBlob arbitrary, fmap GitLookupBlob arbitrary]
  shrink (GitWriteBlob blob) = fmap GitWriteBlob $ shrink blob
  shrink (GitLookupBlob oid) = fmap GitLookupBlob $ shrink oid

executeGit::[GitAction] -> [GitTrace]
executeGit actions = given_a_repository $ forM actions . step
  where
    step repo (GitWriteBlob blob) = fmap GitTraceOid $ Git.write repo blob
    step repo (GitLookupBlob oid) = fmap GitTraceBlob $ Git.lookup repo oid
  
verifyTrace::[GitAction] -> [GitTrace] -> Bool 
verifyTrace actions trace = fst $ foldl step (True, Map.empty) (zip actions trace)
  where
    step (False, st) _ = (False, st)
    step (_, state) (GitWriteBlob blob, GitTraceOid (Just oid)) = (True, Map.insert oid blob state)
    step (_, state) (GitLookupBlob oid, GitTraceBlob blob) = (Map.lookup oid state == blob, state)
        
prop_blob_writes_and_lookups_produce_a_valid_trace actions = 
  verifyTrace actions $ executeGit actions

tests = [ run "blob write and lookup trace" prop_blob_writes_and_lookups_produce_a_valid_trace ]

