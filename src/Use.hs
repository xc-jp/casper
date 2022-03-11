{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Use where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Lib

data Root mut imm = Root (mut (Foo mut imm)) (mut (Foo mut imm))

data Foo mut (imm :: Type -> Type) = Foo
  { mi :: mut Int,
    mli :: mut [Int],
    lmi :: [mut Int],
    rec :: mut (Foo mut imm),
    recs :: mut [Foo mut imm],
    val :: Int
  }
  deriving (Generic)

instance Content mut imm (Foo mut imm)

someFunc :: IO ()
someFunc =
  loadStore "/dev/null" $ do
    transact $ \(Root l r) -> do
      foo1 <- readMut l
      foo2 <- readMut (rec foo1)
      readMut (mi foo1)
    pure ()
