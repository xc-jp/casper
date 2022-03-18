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
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Kind (Type)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Root s = Root (Var s (Foo s)) (Var s (Foo s))
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Root s)

data Foo s = Foo
  { mi :: Var s Int,
    mli :: Var s [Int],
    lmi :: [Var s Int],
    rec :: Var s (Foo s),
    recs :: Var s [Foo s],
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Foo s)

deriving instance Content s (Foo s)

-- >>> import qualified Data.Aeson as Aeson
-- >>> Aeson.encode exampleFoo
-- "{\"mi\":\"00000000-0000-0000-0000-000000000000\",\"val\":2,\"mli\":\"00000000-0000-0000-0000-000000000000\",\"recs\":\"00000000-0000-0000-0000-000000000000\",\"rec\":\"00000000-0000-0000-0000-000000000000\",\"lmi\":[\"00000000-0000-0000-0000-000000000000\",\"00000000-0000-0000-0000-000000000000\"]}"
exampleFoo :: Foo s
exampleFoo = Foo fakeVar fakeVar [fakeVar, fakeVar] fakeVar fakeVar 2

-- data Dataset i = Dataset ([i (Datapoint i)])
--   deriving (Generic)

-- instance Content m i (Dataset i)

-- data Datapoint i = DataPoint (i Int)

{-
  t1 = transaction:
      add set_A
        add datapoint_A_A
  t2 = transaction:
      add set_B
        add datapoint_B_A
      delete set_A
  t3 = transaction:
     modify set_A
       add datapoint_A_B

-}

-- Foo Loc Ref' ===> Foo Loc (Ref s)

-- root
--   models
--     model_A

-- root
--   datasets
--     set_A
--       datapoint_A_A
--       datapoint_A_B
--     set_B
--       datapoint_B_A
--       datapoint_B_B

-- instance Content mut imm (Foo mut imm)

someFunc :: IO Int
someFunc =
  loadStore "/dev/null" $ do
    transact $ \(Root l _) -> do
      foo1 <- readVar l
      foo2 <- readVar (rec foo1)
      readVar (mi foo2)

-- localRoot (\(Root l r) -> rec <$> readMut l) $ do
--   liftIO $ putStrLn "Changed root"
--   pure ()
