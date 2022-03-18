{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Use where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Kind (Type)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Lib

data Root s = Root (RRef s (Foo s)) (RRef s (Foo s))
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Root s)

data Foo s = Foo
  { mi :: RRef s Int,
    mli :: RRef s [Int],
    lmi :: [RRef s Int],
    rec :: RRef s (Foo s),
    recs :: RRef s [Foo s],
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Foo s)

deriving instance Content s (Foo s)

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

someFunc :: IO ()
someFunc =
  loadStore "/dev/null" $ do
    transact $ \(Root l r) -> do
      foo1 <- readMut l
      foo2 <- readMut (rec foo1)
      readMut (mi foo1)
    -- localRoot (\(Root l r) -> rec <$> readMut l) $ do
    --   liftIO $ putStrLn "Changed root"
    --   pure ()
    pure ()
