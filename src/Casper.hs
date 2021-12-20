module Casper
  ( -- * Content addressable context
    CasperT,
    CasperError,
    runCasperT,
    catchError,

    -- * Store
    Store,
    initStore,
    collectGarbage,

    -- * Content
    Ref,
    Content (..),
    JSONContent,
    SHA256 (..),
    store,
    retrieve,
    forget,
    recall,

    -- * Resources
    Loc,
    Resource (..),
    newResource,
    readResource,
    writeResource,
    modifyResource,
    spot,
    getRoot,

    -- * Hashing
    hashBS,
  )
where

import Casper.Content
import Casper.Internal
import Casper.Resource
import Control.Monad.Except (catchError)
