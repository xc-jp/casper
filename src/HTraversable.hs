{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module HTraversable where

import Control.Applicative
import GHC.Generics

class HBitraversable (t :: (a -> *) -> (b -> *) -> *) where
  hbitraverse ::
    Applicative m =>
    (forall x y. f x -> m (f' y)) ->
    (forall x y. g x -> m (g' y)) ->
    (t f g -> m (t f' g'))
  default hbitraverse ::
    ( Applicative m,
      Generic (t f g),
      Generic (t f' g'),
      (GHBitraversal f f' g g' (Rep (t f g)) (Rep (t f' g')))
    ) =>
    (forall x y. f x -> m (f' y)) ->
    (forall x y. g x -> m (g' y)) ->
    (t f g -> m (t f' g'))
  hbitraverse f g t = to <$> ghbitraverse f g (from t)

class HTraversable (t :: (k -> *) -> *) where
  htraverse ::
    (forall x. f x -> m (f' x)) ->
    (t f -> m (t f'))

class GHBitraversal f f' g g' t t' where
  ghbitraverse ::
    Applicative m =>
    (forall x y. f x -> m (f' y)) ->
    (forall x y. g x -> m (g' y)) ->
    (t k -> m (t' k))

data Test f g = Test (f Int) (g Int) (f (f Int)) Int
  deriving stock (Generic)
  deriving anyclass (HBitraversable)

instance {-# INCOHERENT #-} GHBitraversal f f' g g' (K1 c (f x)) (K1 c (f' y)) where ghbitraverse ff _ (K1 f) = K1 <$> ff f

instance {-# INCOHERENT #-} GHBitraversal f f' g g' (K1 c (g x)) (K1 c (g' y)) where ghbitraverse _ fg (K1 g) = K1 <$> fg g

instance {-# OVERLAPPABLE #-} GHBitraversal f f' g g' (K1 c a) (K1 c a) where ghbitraverse _ _ (K1 a) = pure $ K1 a

-- instance GHBitraversal f f' g g' g g' where ghbitraverse ff fg g = fg g

instance
  GHBitraversal f f' g g' t t' =>
  GHBitraversal f f' g g' (M1 i c t) (M1 i c t')
  where
  ghbitraverse ff fg (M1 t) = M1 <$> ghbitraverse ff fg t

instance
  (GHBitraversal f f' g g' l l', GHBitraversal f f' g g' r r') =>
  GHBitraversal f f' g g' (l :*: r) (l' :*: r')
  where
  ghbitraverse ff fg (l :*: r) = liftA2 (:*:) (ghbitraverse ff fg l) (ghbitraverse ff fg r)
