{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rescope where

import Data.Coerce
import GHC.Generics
import Ref (Ref (..))
import Var (Var (..))

class Rescope a b where
  -- | Don't expose to the user
  rescope :: a -> b
  default rescope :: Coercible a b => a -> b
  rescope = unCoercibleRescope . rescope . CoercibleRescope
  {-# INLINE rescope #-}

-- * Coercible wrapper

-- This already is the default behavior, but having a newtype wrapper probably can't hurt
newtype CoercibleRescope a = CoercibleRescope {unCoercibleRescope :: a}

instance Coercible a b => Rescope a (CoercibleRescope b) where
  {-# INLINE rescope #-}
  rescope a = CoercibleRescope (coerce a)

-- * Functor wrapper

newtype FunctorRescope a = FunctorRescope a

instance (Functor f, Rescope a b) => Rescope (f a) (FunctorRescope (f b)) where
  {-# INLINE rescope #-}
  rescope = FunctorRescope . fmap rescope

-- * Generic wrapper

newtype GenericRescope a = GenericRescope a

instance (Generic a, Generic b, GRescope (Rep a) (Rep b)) => Rescope a (GenericRescope b) where
  {-# INLINE rescope #-}
  rescope a = GenericRescope $ to $ grescope (from a)

class GRescope a b where
  grescope :: a x -> b x

instance Rescope a b => GRescope (K1 c a) (K1 c' b) where grescope (K1 a) = K1 $ rescope a

instance GRescope a b => GRescope (M1 i c a) (M1 i c' b) where grescope (M1 a) = M1 $ grescope a

instance (GRescope a c, GRescope b d) => GRescope (a :*: b) (c :*: d) where grescope (a :*: b) = grescope a :*: grescope b

instance (GRescope a c, GRescope b d) => GRescope (a :+: b) (c :+: d) where
  grescope (L1 a) = L1 (grescope a)
  grescope (R1 b) = R1 (grescope b)

instance GRescope U1 U1 where grescope = id

instance GRescope V1 V1 where grescope = id

-- * Instances

deriving via (CoercibleRescope (Ref b t)) instance Rescope a b => Rescope (Ref a s) (Ref b t)

deriving via (CoercibleRescope (Var b t)) instance Rescope a b => Rescope (Var a s) (Var b t)

-- or, equivalently
-- instance Rescope a b => Rescope (Var a s) (Var b t)
-- instance Rescope a b => Rescope (Ref a s) (Ref b t)

deriving via (FunctorRescope [b]) instance Rescope a b => Rescope [a] [b]

-- * Identity instances

-- This does seem to mess up Use.hs
-- instance {-# OVERLAPPABLE #-} Rescope a a where rescope = id

instance Rescope Int Int

instance Rescope Double Double
