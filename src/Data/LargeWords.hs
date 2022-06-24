{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.LargeWords where

import Data.Hashable (Hashable)
import Data.Word (Word64)
import GHC.Generics (Generic)

data Word128
  = Word128
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

data Word256
  = Word256
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)
