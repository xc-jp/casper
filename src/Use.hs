{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Use where

import Casper
import Control.Applicative ((<|>))
import Control.Lens (at, (?~))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.String (fromString)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics

data Root s = Root [Var (Foo s) s] [Var (Bar s) s]
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Root s)

data Foo s = Foo
  { mi :: Var Int s,
    _mli :: Var [Int] s,
    lmi :: [Var Int s],
    rec :: Var (Foo s) s,
    recs :: Var [Foo s] s,
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Typeable)
  deriving (JSONMeta)

deriving instance Content (Foo s)

data Bar s = Bar
  { barbie :: Int,
    ken :: Var (Bar s) s
  }

-- TODO what the fuck how do we prevent this
whatTheFuckHowDoWePreventThis :: Var Int a -> Var Int b
whatTheFuckHowDoWePreventThis = coerce

fooType :: String
fooType = show $ typeRep (Proxy :: Proxy (Foo ()))

-- >>> import qualified Data.Aeson as Aeson
-- >>> Serialize.runPut (Serialize.put exampleFoo)
-- "\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL{\"mi\":\"00000000-0000-0000-0000-000000000000\",\"val\":2,\"mli\":\"00000000-0000-0000-0000-000000000000\",\"meta\":{\"meta\":{\"constructor\":\"Foo\",\"meta\":[[{\"meta\":null,\"field\":\"mi\"},[{\"meta\":null,\"field\":\"_mli\"},{\"meta\":null,\"field\":\"lmi\"}]],[{\"meta\":null,\"field\":\"rec\"},[{\"meta\":null,\"field\":\"recs\"},{\"meta\":null,\"field\":\"val\"}]]]},\"type\":\"Foo\"},\"recs\":\"00000000-0000-0000-0000-000000000000\",\"rec\":\"00000000-0000-0000-0000-000000000000\",\"lmi\":[\"00000000-0000-0000-0000-000000000000\",\"00000000-0000-0000-0000-000000000000\"]}"
exampleFoo :: Foo s
exampleFoo = Foo fakeVar fakeVar [fakeVar, fakeVar] fakeVar fakeVar 2

someFunc :: IO Int
someFunc =
  loadStore "/dev/null" $ \(Root l _) -> do
    transact $ do
      foo1 <- readVar l
      foo2 <- readVar (rec foo1)
      writeVar (mi foo2) 0
    borrow (readVar l >>= readVar . rec) $ \l' -> do
      transact $ readVar (mi l')
