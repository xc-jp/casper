# Casper: a content-addressable store

`Casper` is a content addresable store using ordinary directories, and
content-addressible files (files named after the SHA-1 hash of the file's
conetnt) in the file system as the data store, similar to how the Git revision
control system stores information. All filesystem access through a single
`Store` handle in a single Haskell process is guaranteed to be thread safe by
the STM library. However **no guarantees** are provided as to the consistency
of the data store outside of the `Store` handle of a single Haskell process,
for example, if an application were to create multiple `Store`s to the same
filesystem directory accessible by multiple threads, or if multiple processes
were to access a single Casper store.

The `openStore` function is the entry point to the Casper data store, which
evaluates functions of type `CasperT`. The `Store` data type itself, and all
types stored within (such as `Ref` and `Var`) have an `s` type variable which
serves the same purpose as the `s` type variable of the `Control.Monad.ST.ST`
function type: to prevent conflicting stateful updates to the Store content. It
is expected that a Casper store will be opened with `openStore` in a separate
process thread only once, at process initialization, and a function in the
`CasperT` monad will loop forever, listining for requests on an incoming
channel for data store lookups and updates.

To actually store and retrieve data with Casper, start by defining a `root`
data type that can contain all of the information that is stored in the data
store, such as a list or a tree of immutable `Ref` values or mutable `Var`
values. Both `Var` and `Ref` data types can themselves be stored into other
data types that can be written to the store. By traversing through the `root`
data type, resolving `Ref`s and `Var`s as you go, you can extract pure data
types from the store You can also fold in the other direction, converting pure
data types to storable data types.

The `root` data type is stored into a single object in the data store, and
contains an index to all other objects stored in the data store. This `root`
object given to you by `openStore` is always a mutable `Var`, otherwise with a
`Ref` you would only be able to have a read-only data store. The `root` data
type must take at least one type variable `s` which serves a similar purpose to
the `s` value of the `Control.Monad.ST.ST` monad.

It also is necessary for the `root` data type to derive several type classes,
at a minimum:

  - `Content` from `Casper` in this `casper` package
  - `Generic` from `GHC.Generics` in `base`
  - `FromJSON` and `ToJSON` from `Data.Aeson` in the `aeson` package
  - `Serialize` from `Data.Serialize` in the `cereal` package

The easiest way to instantiate these type classes is with the GHC language
extensions:

  - `DeriveGeneric`
  - `DeriveAnyClass`
  - `DerivingStrategies`
  - `DerivingVia`

Casper content is stored as `ByteString`s in the data store. The `Casper`
module provides a `WrapAeson` newtype that instantiates `Serialize` in such a
way that data is serialized as a JSON string without prefixing the string with
a binary integer string length. See the `WrapAeson` data type for more
information.

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
