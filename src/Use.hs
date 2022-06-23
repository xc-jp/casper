{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Use where

import Casper
import Data.Aeson (FromJSON, ToJSON)
import Data.Serialize (Serialize)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Root = Root [Var Foo] [Var Bar]
  deriving stock (Generic)
  deriving (Content)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson Root

data Foo = Foo
  { mi :: Var Int,
    _mli :: Var [Int],
    lmi :: [Var Int],
    rec :: Var Foo,
    recs :: Var [Foo],
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Typeable, Content, Eq, Show)
  deriving (Serialize) via WrapAeson Foo

data Bar = Bar
  { barbie :: Int,
    ken :: Var Bar
  }

-- >>> import qualified Data.Serialize as Serialize
-- >>> let bytes = Serialize.runPut (Serialize.put exampleFoo)
-- >>> bytes
-- >>> let exampleFoo' = Serialize.runGet Serialize.get bytes :: Either String (Foo)
-- >>> let exampleFoo'' = fmap (\f -> f {val = 3}) exampleFoo'
-- >>> Right exampleFoo == exampleFoo'
-- >>> Right exampleFoo == exampleFoo''
-- >>> error $ show exampleFoo'
-- >>> error $ show exampleFoo
-- "{\"_mli\":\"00000000-0000-0000-0000-000000000000\",\"lmi\":[\"00000000-0000-0000-0000-000000000000\",\"00000000-0000-0000-0000-000000000000\"],\"mi\":\"00000000-0000-0000-0000-000000000000\",\"rec\":\"00000000-0000-0000-0000-000000000000\",\"recs\":\"00000000-0000-0000-0000-000000000000\",\"val\":2}"
-- True
-- False
-- Right (Foo {mi = 00000000-0000-0000-0000-000000000000, _mli = 00000000-0000-0000-0000-000000000000, lmi = [00000000-0000-0000-0000-000000000000,00000000-0000-0000-0000-000000000000], rec = 00000000-0000-0000-0000-000000000000, recs = 00000000-0000-0000-0000-000000000000, val = 2})
-- Foo {mi = 00000000-0000-0000-0000-000000000000, _mli = 00000000-0000-0000-0000-000000000000, lmi = [00000000-0000-0000-0000-000000000000,00000000-0000-0000-0000-000000000000], rec = 00000000-0000-0000-0000-000000000000, recs = 00000000-0000-0000-0000-000000000000, val = 2}
exampleFoo :: Foo
exampleFoo = Foo fakeVar fakeVar [fakeVar, fakeVar] fakeVar fakeVar 2

-- >>> show fakeRef
-- "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
-- >>> import Ref(SHA)
-- >>> import Var(UUID)
-- >>> import Text.Read(readEither)
-- >>> readEither (show fakeVar) :: Either String UUID
-- >>> readEither (show fakeRef) :: Either String SHA
-- Right 00000000-0000-0000-0000-000000000000
-- Right AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
someFunc :: IO Int
someFunc =
  openStore "/dev/null" (Root [] []) $ \rootVar -> transact $ do
    (Root ls _) <- readVar rootVar
    loop ls 0
  where
    loop [] z = pure z
    loop (l : ls) z = do
      foo1 <- readVar l
      foo2 <- readVar (rec foo1)
      writeVar (mi foo2) 0
      loop ls (z + val foo1)
