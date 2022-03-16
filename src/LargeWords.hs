{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module LargeWords where

import Data.Bits (Bits (shiftL), shiftR)
import Data.Hashable (Hashable)
import Data.Word (Word64)
import GHC.Generics (Generic)

data Word128
  = Word128
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- the instances below are not respecting negative numbers correctly but I wanted
-- these for convenience
instance Enum Word128 where
  toEnum i = fromInteger (toInteger i)
  fromEnum w = fromInteger (toInteger w)

instance Num Word128 where
  fromInteger i = Word128 (fromInteger $ i `shiftR` 64) (fromInteger i)
  a + b = fromInteger (toInteger a + toInteger b)
  a - b = fromInteger (toInteger a - toInteger b)
  a * b = fromInteger (toInteger a * toInteger b)
  abs a = fromInteger (abs (toInteger a))
  signum a = fromInteger (signum (toInteger a))

instance Ord Word128 where
  compare a b = compare (toInteger a) (toInteger b)

instance Real Word128 where
  toRational w = toRational (toInteger w)

instance Integral Word128 where
  toInteger (Word128 a b) = (toInteger a `shiftL` 64) + toInteger b
  quotRem v w =
    let (q, r) = quotRem (toInteger v) (toInteger w)
     in (fromInteger q, fromInteger r)

data Word256
  = Word256
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

instance Enum Word256 where
  toEnum i = fromInteger (toInteger i)
  fromEnum w = fromInteger (toInteger w)

instance Num Word256 where
  fromInteger i = Word256 (fromInteger $ i `shiftR` (64 * 3)) (fromInteger $ i `shiftR` (64 * 2)) (fromInteger $ i `shiftR` 64) (fromInteger i)
  a + b = fromInteger (toInteger a + toInteger b)
  a - b = fromInteger (toInteger a - toInteger b)
  a * b = fromInteger (toInteger a * toInteger b)
  abs a = fromInteger (abs (toInteger a))
  signum a = fromInteger (signum (toInteger a))

instance Ord Word256 where
  compare a b = compare (toInteger a) (toInteger b)

instance Real Word256 where
  toRational w = toRational (toInteger w)

instance Integral Word256 where
  toInteger (Word256 a b c d) =
    (toInteger a `shiftL` (64 * 3))
      + (toInteger b `shiftL` (64 * 2))
      + (toInteger c `shiftL` 64)
      + toInteger d
  quotRem v w =
    let (q, r) = quotRem (toInteger v) (toInteger w)
     in (fromInteger q, fromInteger r)
