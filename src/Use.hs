{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Use where

import Content (Content, Refs, WrapAeson (..))
import Data.Aeson (FromJSON, ToJSON (toJSON), parseJSON)
import Data.Functor.Const (Const (Const))
import GHC.Generics (Generic)
import Lib

data Root mut imm = Root (mut (Foo mut imm)) (mut (Foo mut imm))
  deriving (Generic)

instance Refs mut imm (Root mut imm)

instance Content mut imm (Root mut imm)

data Foo mut imm = Foo
  { mi :: mut Int,
    mli :: mut [Int],
    lmi :: [mut Int],
    rec :: mut (Foo mut imm),
    recs :: mut [Foo mut imm],
    imm :: imm (Foo mut imm),
    val :: Int
  }
  deriving (Generic)

deriving via (WrapAeson mut imm (Foo mut imm)) instance Refs mut imm (Foo mut imm)

deriving via (WrapAeson (Const UUID) (Const SHA) (Foo (Const UUID) (Const SHA))) instance Content (Const UUID) (Const SHA) (Foo (Const UUID) (Const SHA))

exampleFoo :: Foo (Const UUID) (Const SHA)
exampleFoo =
  Foo
    { mi = Const uuid0,
      mli = Const uuid1,
      lmi = [Const uuid0, Const uuid1],
      rec = Const uuid2,
      recs = Const uuid2,
      imm = Const sha0,
      val = 3
    }

uuid0, uuid1, uuid2 :: UUID
uuid0 = UUID 0
uuid1 = UUID 1
uuid2 = UUID 2

sha0 :: SHA
sha0 = SHA 0

-- >>> import qualified Data.Aeson as Aeson
-- >>> Aeson.encode exampleFoo
-- "{\"mi\":0,\"val\":3,\"mli\":1,\"recs\":2,\"rec\":2,\"imm\":0,\"lmi\":[0,1]}"
--

instance ToJSON UUID where
  toJSON (UUID u) = toJSON (toInteger u)

instance FromJSON UUID where
  parseJSON v = UUID . fromInteger <$> parseJSON v

instance ToJSON SHA where
  toJSON (SHA u) = toJSON (toInteger u)

instance FromJSON SHA where
  parseJSON v = SHA . fromInteger <$> parseJSON v

instance ToJSON (Foo (Const UUID) (Const SHA))

instance FromJSON (Foo (Const UUID) (Const SHA))

newtype Dataset i = Dataset ([i (Datapoint i)])
  deriving (Generic)

newtype Datapoint i = DataPoint (i Int)

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

{-
someFunc :: IO Int
someFunc =
  loadStore "/dev/null" $ do
    transact $ \(Root l r) -> do
      foo1 <- readMut l
      foo2 <- readMut (rec foo1)
      readMut (mi foo1)
      -}

{-
localRoot (\(Root l r) -> rec <$> readMut l) $ do
  liftIO $ putStrLn "Changed root"
  pure ()
pure ()
-}
