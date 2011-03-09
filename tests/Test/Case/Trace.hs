module Test.Case.Trace where

import qualified Git
import Control.Monad
import Test.Util
import Test.Arbitrary
import qualified Data.Map as Map
import Data.Maybe
import Test.QuickCheck

data Action = WriteBlob Git.Blob
            | LookupBlob Git.Oid
            | LookupExistingBlob Int
                 deriving (Show, Eq)
                          
data Trace = TraceLookup { lookup_oid:: Git.Oid
                   , found_blob::Maybe Git.Blob }
           | TraceWrite { saved_blob::Git.Blob 
                      , got_oid::Maybe Git.Oid }
           | TraceNop
           deriving (Show, Eq)
                
instance Arbitrary Action where
  arbitrary = oneof [fmap WriteBlob arbitrary
                    , fmap LookupBlob arbitrary
                    , fmap LookupExistingBlob arbitrary]
  shrink (WriteBlob blob) = fmap WriteBlob $ shrink blob
  shrink (LookupBlob oid) = fmap LookupBlob $ shrink oid
  shrink (LookupExistingBlob idx) = fmap LookupExistingBlob $ shrink idx

execute::[Action] -> [Trace]
execute actions = reverse $ given_a_repository $ \repo -> foldM (step repo) [] actions
  where
    step repo result (WriteBlob blob) = ((:result) . TraceWrite blob) `fmap` Git.write repo blob
    step repo result (LookupBlob oid) = ((:result) . TraceLookup oid) `fmap`  Git.lookup repo oid
    step repo result (LookupExistingBlob idx) = 
      case drop idx writtenBlobOids of
        [] -> return (TraceNop:result)
        oid:_ -> do
          print $ ">>> " ++ show oid
          ((:result) . TraceLookup oid) `fmap`  Git.lookup repo oid
      where 
        writtenBlobOids = [fromJust got_oid | TraceWrite {saved_blob, got_oid} <- result ]
    
verify::[Trace] -> Bool 
verify trace = fst $ foldl step (True, Map.empty) trace
  where
    step (False, state) _ = (False, state)
    step (True, state) TraceNop = (True, state)
    step (True, state) TraceWrite { saved_blob, got_oid } = 
      (True, Map.insert (fromJust got_oid) saved_blob state)
    step (True, state) TraceLookup { lookup_oid, found_blob} = 
      (Map.lookup lookup_oid state == found_blob, state)
                                                             
prop_blob_writes_and_lookups_produce_a_valid_trace actions = 
  verify $ execute actions

tests = [ run "blob write and lookup trace" prop_blob_writes_and_lookups_produce_a_valid_trace ]

