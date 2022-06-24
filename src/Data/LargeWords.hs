{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.LargeWords where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (get, put))
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

instance Serialize Word128 where
  put (Word128 big little) = put big *> put little
  get = liftA2 Word128 get get

instance Serialize Word256 where
  put (Word256 a b c d) = put a *> put b *> put c *> put d
  get = Word256 <$> get <*> get <*> get <*> get
