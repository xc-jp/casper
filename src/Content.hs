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

module Content (Content (..)) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Kind (Type)
import GHC.Generics

class Refs (mut :: Type -> Type) imm a where
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

instance
  (ToJSON a, FromJSON a, Generic a, GMutableContent mut imm (Rep a)) =>
  Content mut imm (WrapAeson mut imm a)
  where
  refs fm fi (WrapAeson a) = grefs fm fi (from a)
  encode _ _ = BL.toStrict . Aeson.encode . unWrapAeson
  decode _ _ = fmap WrapAeson . Aeson.eitherDecodeStrict'

deriving via (WrapAeson mut imm Int) instance Content mut imm Int

newtype WrapCereal mut imm a = WrapCereal {unWrapCereal :: a}

instance Content mut imm (mut a) where
  refs fm _ m = [fm m]
  encode fm _ = fm
  decode fm _ = fm

instance Content mut imm (imm a) where
  refs _ fi m = [fi m]
  encode _ fi = fi
  decode _ fi = fi

class GMutableContent mut imm a where
  grefs :: (forall r. mut r -> ref) -> (forall r. imm r -> ref) -> a x -> [ref]

instance GMutableContent mut imm U1 where
  grefs _ _ _ = []

instance Content mut imm a => GMutableContent mut imm (K1 m a) where
  grefs fm fi (K1 a) = refs fm fi a

instance GMutableContent mut imm f => GMutableContent mut imm (M1 i c f) where
  grefs fm fi (M1 a) = grefs fm fi a

instance (GMutableContent mut imm f, GMutableContent mut imm g) => GMutableContent mut imm (f :*: g) where
  grefs fm fi (l :*: r) = grefs fm fi l <> grefs fm fi r

instance (GMutableContent mut imm f, GMutableContent mut imm g) => GMutableContent mut imm (f :+: g) where
  grefs fm fi (L1 l) = grefs fm fi l
  grefs fm fi (R1 r) = grefs fm fi r

instance Content mut imm a => Content mut imm [a]
