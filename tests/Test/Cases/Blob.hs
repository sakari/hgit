module Test.Cases.Blob where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Util
import qualified Git.Blob as Blob

tests::Test
tests = testGroup "Test.Cases.Blob" 
        [ testProperty "lookup non existing blob" $ \oid -> with_repo $ \repo -> do
             fails $ Blob.lookup repo oid
             
        , testProperty "write and lookup a blob" $ \contents -> with_repo $ \repo -> do
             oid <- Blob.write repo contents
             blob <- Blob.lookup repo oid
             assertEqual contents blob
             
        , testProperty "writing a blob is idempotent" $ \contents -> with_repo $ \repo -> do
             oid <- Blob.write repo contents
             oid' <- Blob.write repo contents
             blob <- Blob.lookup repo oid
             _ <- assertEqual oid oid'
             assertEqual contents blob
        
        , testProperty "blob is found after writing any other blob" $ 
          \blob_l blob blob_r -> with_repo $ \repo -> do
            let haystack = blob_l ++ [blob] ++ blob_r
                targetIndex = length blob_l
            oids <- Blob.write repo `mapM` haystack
            foundBlob <- Blob.lookup repo $ oids !! targetIndex 
            blob `assertEqual` foundBlob
        ]
