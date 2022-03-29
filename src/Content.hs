{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Content where

import GHC.Generics
import Ref (Ref)
import Var (Var)

class Content s a where
  refs ::
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent s (Rep a)) =>
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

class GContent s a where
  grefs ::
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a x -> [ref])

instance Content s a => GContent s (K1 c a) where grefs fr fc (K1 a) = refs fr fc a

instance GContent s a => GContent s (M1 i c a) where grefs fr fc (M1 a) = grefs fr fc a

instance (GContent s a, GContent s b) => GContent s (a :*: b) where grefs fr fc (a :*: b) = grefs fr fc a <> grefs fr fc b

instance (GContent s a, GContent s b) => GContent s (a :+: b) where
  grefs fr fc (L1 a) = grefs fr fc a
  grefs fr fc (R1 b) = grefs fr fc b

instance GContent s U1 where grefs _ _ _ = []

instance GContent s V1 where grefs _ _ _ = []

instance Content s (Ref a s) where refs _ fc c = pure $ fc c

instance Content s (Var a s) where refs fr _ r = pure $ fr r

instance Content s a => Content s [a]

instance Content s Int where refs _ _ _ = []
