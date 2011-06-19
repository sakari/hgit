{-# OPTIONS_GHC -w #-}

{-| Git for Haskell on top of a shallow wrapper ( "Bindings.Libgit2" ) for libgit2.

A short introductory example follows.

First we initialize a non-bare repository @example@ and write a file
named @foobar@ under the repository 'Git.Repository.workdir'. The
'Git.Types.unsafePathToEntry' is used for producing valid names for
entries to index. For a safe variant see 'Git.Types.pathToEntry'.

>>> repo <- Git.Repository.init "example"
>>> :m + Data.ByteString
>>> Git.Repository.writeFile repo (Git.Types.unsafePathToEntry "foobar") $ singleton $ toEnum 0

Next we load the repository index to memory and stage @foobar@

>>> index <- Git.Index.open repo
>>> let stage = 1::Int
>>> Git.Index.addFile index (Git.Types.unsafePathToEntry "foobar") stage

For creating a commit we store the index as a tree to the object
store, and define an author and a committer.

>>> treeOid <- Git.Tree.fromIndex index
>>> let author = Git.Commit.Signature "tester" "tester@example.com" $ Git.Commit.Time 0 0
>>> let committer = author

Now when we create the commit we get the 'Git.Oid.Oid' as a result. 

>>> commitOid <- Git.Commit.create repo Nothing author committer "message" treeOid []

Using the returned @commitOid@ we can retrieve the commit again

>>> commit <- Git.Commit.lookup repo commitOid
>>> Git.Commit.signature_author $ Git.Commit.author commit
"tester"
-}

module Git where
import qualified Git.Types
import qualified Git.Repository
import qualified Git.Commit
import qualified Git.Index
import qualified Git.Tree
import qualified Git.Oid
import qualified Git.TreeBuilder
