# Casper: a content-addressable store

`Casper` is a content addressable store using ordinary directories, and
content-addressable files (files named after the SHA256 hash of the file's
content) in the file system, similar to how the Git revision control system
stores information.

All file system access through a single `Store` handle in a single Haskell
process is guaranteed to be thread safe by relying on the STM library. However
**no guarantees** are provided as to the consistency of the data store outside
of the `Store` handle of a single Haskell process, for example, if an
application were to create multiple `Store`s to the same file system directory
accessible by multiple threads, or if multiple processes were to access
a single Casper store.

The `openStore` function is the entry point to the Casper data store, which
creates a handle to the store and evaluates transaction on it. The `Store` data
type itself, and all types stored within (such as `Ref` and `Var`) have an `s`
type variable which serves the same purpose as the `s` type variable of the
`Control.Monad.ST.ST` type: to prevent to limit the scope of what the stateful
actions are allowed to update. It is expected that a Casper store will be
opened with `openStore` only **once per process**, at process initialization.

Casper allows you to write any Haskell data into the store provided you can
serialize them and list all other pieces of data they refer to. This is done
using the `Serialize` (from cereal) and `Content` (from casper) type classes.

You cannot delete anything from the store explicitly, instead anything that is
not accessible from the `root` object is deleted when Casper runs garbage
collection.

The `root` data type is stored into a single object in the data store, and may
contain references ('Ref' and 'Var') to other objects. The transitive closure
of the references starting in this `root` object is the state of the store. The
`root` type must take a type argument `s` to tie its references to the lifetime
of the 'Store'.

Casper content is stored in plain files and serialized to `ByteString`s before
writing to the files in the store. The `Casper` module provides a `WrapAeson`
newtype that instantiates `Serialize` in such a way that data is serialized as
a JSON string of bytes.

See the `WrapAeson` data type for more information.

Here is a template for a program that uses a Casper data store:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Main where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON) -- from the "aeson" package
import Data.Serialize (Serialize) -- from the "cereal" package

data Root s
    = Root
      { ...
      }
    deriving stock (Generic)
    deriving (Content)
    deriving anyclass (FromJSON, ToJSON)
    deriving (Serialize) via WrapAeson (Root s)

emptyRoot :: Root s
emptyRoot = Root{ ... }

main :: IO ()
main = do
    ...
    openStore "/path/to/store" emptyRoot $ \ currentRoot -> do
       store <- getStore
       liftIO . forkIO . forever . runCasperT store $ do
            -- read/write operations on the Casper store
            item <- hReadIO stdin -- Take some input, parse it
            writeVar currentRoot item -- store the item we got from stdin
        ...
```
