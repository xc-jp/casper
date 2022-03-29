{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Use where

import Casper
import Data.Aeson
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Root s = Root (Var (Foo s) s) (Var (Foo s) s)
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Root s)

data Foo s = Foo
  { mi :: Var Int s,
    mli :: Var [Int] s,
    lmi :: [Var Int s],
    rec :: Var (Foo s) s,
    recs :: Var [Foo s] s,
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Foo s)

deriving instance Content s (Foo s)

deriving instance Rescope (Foo s) (Foo t)

-- >>> import qualified Data.Aeson as Aeson
-- >>> Aeson.encode exampleFoo
-- "{\"mi\":\"00000000-0000-0000-0000-000000000000\",\"val\":2,\"mli\":\"00000000-0000-0000-0000-000000000000\",\"recs\":\"00000000-0000-0000-0000-000000000000\",\"rec\":\"00000000-0000-0000-0000-000000000000\",\"lmi\":[\"00000000-0000-0000-0000-000000000000\",\"00000000-0000-0000-0000-000000000000\"]}"
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
      pure (val l')
