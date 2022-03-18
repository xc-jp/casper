{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Content (Content (..)) where

import Control.Applicative
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Kind (Type)
import GHC.Generics
import HTraversable
import LargeWords

class Refs (mut :: Type -> Type) imm a where
  refs ::
    (forall r. mut r -> ref) ->
    (forall r. imm r -> ref) ->
    (a -> [ref])

class Refs mut imm a => Content mut imm a where
  encode ::
    (forall r. mut r -> Word128) ->
    (forall r. imm r -> Word256) ->
    (a -> ByteString)

  decode ::
    (forall r. Word128 -> mut r) ->
    (forall r. Word256 -> imm r) ->
    (ByteString -> Either String a)

newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

newtype WrapAesonMI (mut :: * -> *) (imm :: * -> *) a = WrapAesonMI {unWrapAesonMI :: a mut imm}

instance
  ( Refs mut imm (a mut imm),
    HBitraversable a,
    ToJSON (a (Const Word128) (Const Word256))
  ) =>
  Content mut imm (WrapAesonMI mut imm a)
  where
  encode fm fi (WrapAesonMI a) = undefined

deriving instance Refs mut imm a => Refs mut imm (WrapAeson a)

deriving instance Refs mut imm (a mut imm) => Refs mut imm (WrapAesonMI mut imm a)

instance (Refs mut imm a, ToJSON a, FromJSON a) => Content mut imm (WrapAeson a) where
  encode _ _ = BL.toStrict . Aeson.encode . unWrapAeson
  decode _ _ = fmap WrapAeson . Aeson.eitherDecodeStrict'

newtype NoRefs a = NoRefs {unNoRefs :: a}

instance Refs mut imm (NoRefs a) where
  refs _ _ _ = []

deriving via (NoRefs Int) instance Refs mut imm Int

deriving via (WrapAeson Int) instance Content mut imm Int

newtype WrapCereal mut imm a = WrapCereal {unWrapCereal :: a}

instance Refs mut imm (mut a) where refs _ _ _ = []

instance Refs mut imm (imm a) where refs _ _ _ = []

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
