{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Kind (Type)
import GHC.Generics

data Graph root = Graph

newtype Transaction (mut :: Type -> Type) (imm :: Type -> Type) a = Transaction (Identity a)
  deriving (Functor, Monad, Applicative)

class Content (mut :: Type -> Type) (imm :: Type -> Type) a where
  refs ::
    (forall r. mut r -> ref) ->
    (forall r. imm r -> ref) ->
    (a -> [ref])

  encode ::
    (forall r. mut r -> ByteString) ->
    (forall r. imm r -> ByteString) ->
    (a -> ByteString)

  decode ::
    (forall r. ByteString -> Either String (mut r)) ->
    (forall r. ByteString -> Either String (imm r)) ->
    (ByteString -> Either String a)

  default refs ::
    (Generic a, GMutableContent mut imm (Rep a)) =>
    (forall r. mut r -> ref) ->
    (forall r. imm r -> ref) ->
    a ->
    [ref]
  refs fm fi a = grefs fm fi (from a)

newtype WrapAeson (mut :: Type -> Type) (imm :: Type -> Type) a = WrapAeson {unWrapAeson :: a}

instance (ToJSON a, FromJSON a) => Content mut imm (WrapAeson mut imm a) where
  refs _ _ _ = []
  encode _ _ = BL.toStrict . Aeson.encode . unWrapAeson
  decode _ _ = fmap WrapAeson . Aeson.eitherDecodeStrict'

deriving via (WrapAeson mut imm Int) instance Content mut imm Int

newtype WrapCereal mut imm a = WrapCereal {unWrapCereal :: a}

newtype CasperT s root m a = CasperT (m a)
  deriving (Functor, Monad, Applicative)

loadStore :: FilePath -> (forall s. CasperT s root m a) -> m a
loadStore _ _ = undefined

transact :: (forall mut imm. root mut imm -> Transaction mut imm a) -> CasperT s root IO a
transact _ = undefined

localRoot ::
  ( forall mut imm.
    root mut imm ->
    Transaction mut imm (mut (root' mut imm))
  ) ->
  CasperT s root' m a ->
  CasperT s root m a
localRoot _ = undefined

readMut :: mut a -> Transaction mut imm a
readMut _ = undefined

writeMut :: mut a -> a -> Transaction mut imm ()
writeMut _ _ = undefined

newMut :: a -> Transaction mut imm (mut a)
newMut = undefined

readImm :: imm a -> Transaction mut imm a
readImm = undefined

newImm :: a -> Transaction mut imm (imm a)
newImm = undefined

class GMutableContent mut imm a where
  grefs :: (forall r. mut r -> ref) -> (forall r. imm r -> ref) -> a x -> [ref]

instance Content mut imm (mut a) where
  refs fm _ m = [fm m]
  encode fm _ = fm
  decode fm _ = fm

instance Content mut imm (imm a) where
  refs _ fi m = [fi m]
  encode _ fi = fi
  decode _ fi = fi

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
