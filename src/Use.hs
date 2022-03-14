{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Use where

import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Lib

data Root mut imm = Root (mut (Foo mut imm)) (mut (Foo mut imm))

data Foo mut imm = Foo
  { mi :: mut Int,
    mli :: mut [Int],
    lmi :: [mut Int],
    rec :: mut (Foo mut imm),
    recs :: mut [Foo mut imm],
    val :: Int
  }
  deriving (Generic)

data Dataset i = Dataset ([i (Datapoint i)])
  deriving (Generic)

instance Content m i (Dataset i)

data Datapoint i = DataPoint (i Int)

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

instance Content mut imm (Foo mut imm)

someFunc :: IO ()
someFunc =
  loadStore "/dev/null" $ do
    transact $ \(Root l r) -> do
      foo1 <- readMut l
      foo2 <- readMut (rec foo1)
      readMut (mi foo1)
    localRoot (\(Root l r) -> rec <$> readMut l) $ do
      liftIO $ putStrLn "Changed root"
      pure ()
    pure ()
