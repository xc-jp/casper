{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Content (Refs (..), Content (..), WrapAeson (..)) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Kind (Type)
import GHC.Generics
  ( Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    U1,
    type (:*:) (..),
    type (:+:) (..),
  )

class Refs (mut :: Type -> Type) (imm :: Type -> Type) a where
  refs ::
    (forall r. mut r -> ref) ->
    (forall r. imm r -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GRefs mut imm (Rep a)) =>
    (forall r. mut r -> ref) ->
    (forall r. imm r -> ref) ->
    a ->
    [ref]
  refs fm fi a = grefs fm fi (from a)

class Refs mut imm a => Content mut imm a where
  encode ::
    (forall r. mut r -> ByteString) ->
    (forall r. imm r -> ByteString) ->
    (a -> ByteString)

  decode ::
    (forall r. ByteString -> Either String (mut r)) ->
    (forall r. ByteString -> Either String (imm r)) ->
    (ByteString -> Either String a)

newtype WrapAeson (mut :: Type -> Type) (imm :: Type -> Type) a = WrapAeson {unWrapAeson :: a}

instance (Refs mut imm a) => Refs mut imm (WrapAeson mut imm a) where
  refs fm fi (WrapAeson a) = refs fm fi a

instance
  (ToJSON a, FromJSON a, Refs mut imm a) =>
  Content mut imm (WrapAeson mut imm a)
  where
  encode _ _ = BL.toStrict . Aeson.encode . unWrapAeson
  decode _ _ = fmap WrapAeson . Aeson.eitherDecodeStrict'

instance Refs mut imm Int where
  refs _ _ _ = []

deriving via (WrapAeson mut imm Int) instance Content mut imm Int

instance Refs mut imm (mut a) where
  refs fm _ m = [fm m]

instance Content mut imm (mut a) where
  encode fm _ = fm
  decode fm _ = fm

instance Refs mut imm (imm a) where
  refs _ fi m = [fi m]

instance Content mut imm (imm a) where
  encode _ fi = fi
  decode _ fi = fi

class GRefs mut imm a where
  grefs :: (forall r. mut r -> ref) -> (forall r. imm r -> ref) -> a x -> [ref]

instance GRefs mut imm U1 where
  grefs _ _ _ = []

instance Refs mut imm a => GRefs mut imm (K1 m a) where
  grefs fm fi (K1 a) = refs fm fi a

instance GRefs mut imm f => GRefs mut imm (M1 i c f) where
  grefs fm fi (M1 a) = grefs fm fi a

instance (GRefs mut imm f, GRefs mut imm g) => GRefs mut imm (f :*: g) where
  grefs fm fi (l :*: r) = grefs fm fi l <> grefs fm fi r

instance (GRefs mut imm f, GRefs mut imm g) => GRefs mut imm (f :+: g) where
  grefs fm fi (L1 l) = grefs fm fi l
  grefs fm fi (R1 r) = grefs fm fi r

instance Refs mut imm a => Refs mut imm [a]
