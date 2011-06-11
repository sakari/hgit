module Test.Cases.Index where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Util
import Test.QuickCheck
import Data.List
import Control.Applicative
import qualified Data.ByteString as ByteString
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
             
        , testProperty "remove a file from index" $ \before path -> with_repo $ \repo -> do
             index <- Index.open repo
             runIndexOp repo index `mapM` before
             if before `addsPath` path then success $ Index.remove index path 
               else fails $ Index.remove index path
               
        , testProperty "addsPath when last is addition" $ \ops name ->
           (ops ++ [Add name ByteString.empty]) `addsPath` name
           
        , testProperty "addsPath when last is removal" $ \ops name ->
           not $ (ops ++ [Remove name]) `addsPath` name
        ]

addsPath ops = maybe False adds . lastOccurrenceOf
  where
    adds Add {} = True
    adds _ = False
    lastOccurrenceOf path  = find ((path ==) . indexOpName ) $ reverse ops
    
data IndexOp = Add { indexOpName::Types.EntryName
                   , indexOpContents::ByteString.ByteString }
             | Remove { indexOpName::Types.EntryName }
             deriving (Show)
                      
instance Arbitrary IndexOp where
  arbitrary = oneof [ Add <$> arbitrary <*> arbitrary
                    , Remove <$> arbitrary
                    ]
  shrink (Add name contents) = tail $ Add <$> (name:shrink name) <*> (contents:shrink contents)
  shrink (Remove name) = Remove <$> shrink name

runIndexOp repo index (Add name contents ) = do
  Repository.writeFile repo name contents
  Index.addFile index name 1  
runIndexOp repo index (Remove name) =
  Index.find index name >>= maybe (return ()) (const $ Index.remove index name)
  
          
withIndex::Testable a => (Index.Index -> IO a) -> Property  
withIndex c = with_repo $ \repo -> Index.open repo >>= c