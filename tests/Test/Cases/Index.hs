module Test.Cases.Index where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Util
import Test.QuickCheck
import qualified Git.Index as Index
import qualified Git.Blob as Blob
import qualified Git.Types as Types
import qualified Git.Repository as Repository
import qualified Data.ByteString as Bytestring

tests = testGroup "Test.Cases.Index" 
        [
          testProperty "lookup non existing index entry" $ \path -> withIndex $ \index -> do
             Index.find index path >>= assertEqual Nothing
        
        , testProperty "lookup an existing index entry" $ \entry -> withIndex $ \index -> do 
             Index.add index entry
             found <- Index.find index $ Index.entry_path entry 
             Just entry `assertEqual` found
             
        , testProperty "add a file to index" $ \path contents -> with_repo $ \repo -> do
             Repository.writeFile repo path contents
             index <- Index.open repo
             Index.addFile index path 1
             Just found <- Index.find index path
             blob <- Blob.lookup repo (Index.entry_oid found)
             assertEqual blob contents
        ]
        
withIndex::Testable a => (Index.Index -> IO a) -> Property  
withIndex c = with_repo $ \repo -> Index.open repo >>= c