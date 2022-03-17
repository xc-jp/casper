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

module Content (Refs (..), TraverseRefs (..), Content (..), WrapAeson (..)) where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Writer (MonadWriter (tell), execWriter)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Data (Proxy (Proxy))
import Data.Functor.Const (Const (Const))
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

instance TraverseRefs f => Refs mut imm (f mut imm) where
  refs as bs f =
    execWriter $
      traverseRefs
        (\_ -> Proxy <$ pure ())
        (\_ -> Proxy <$ pure ())
        (\m -> Proxy <$ tell [as m])
        (\i -> Proxy <$ tell [bs i])
        f

class TraverseRefs f where
  traverseRefs ::
    Applicative m =>
    (forall a b. mut' a -> m (mut' b)) -> -- cast mutable ref
    (forall a b. imm' a -> m (imm' b)) -> -- cast immutable ref
    (forall r. mut r -> m (mut' r)) ->
    (forall r. imm r -> m (imm' r)) ->
    f mut imm ->
    m (f mut' imm')

class Refs mut imm a => Content mut imm a where
  encode ::
    (forall r. mut r -> ByteString) ->
    (forall r. imm r -> ByteString) ->
    (a -> ByteString)

  decode ::
    (forall x y. mut x -> Either String (mut y)) ->
    (forall x y. imm x -> Either String (imm y)) ->
    (forall r. ByteString -> Either String (mut r)) ->
    (forall r. ByteString -> Either String (imm r)) ->
    (ByteString -> Either String a)

newtype WrapAeson (mut :: Type -> Type) (imm :: Type -> Type) a = WrapAeson {unWrapAeson :: a}

instance (Refs mut imm a) => Refs mut imm (WrapAeson mut imm a) where
  refs fm fi (WrapAeson a) = refs fm fi a

castConst :: Const x a -> Const x b
castConst (Const x) = Const x

instance
  ( ToJSON (f (Const ByteString) (Const ByteString)),
    FromJSON (f (Const ByteString) (Const ByteString)),
    TraverseRefs f
  ) =>
  Content mut imm (WrapAeson mut imm (f mut imm))
  where
  encode fm fi =
    BL.toStrict . Aeson.encode
      . runIdentity
      . traverseRefs (pure . castConst) (pure . castConst) (pure . Const . fm) (pure . Const . fi)
      . unWrapAeson
  decode castMut castImm fm fi v = do
    f <- Aeson.eitherDecodeStrict' v
    let parseMut (Const m) = fm m
    let parseImm (Const i) = fi i
    WrapAeson <$> traverseRefs castMut castImm parseMut parseImm f

instance
  ( ToJSON a,
    FromJSON a,
    Refs mut imm a
  ) =>
  Content mut imm (WrapAeson mut imm a)
  where
  encode _ _ = BL.toStrict . Aeson.encode . unWrapAeson
  decode _ _ _ _ v = WrapAeson <$> Aeson.eitherDecodeStrict' v

instance Refs mut imm Int where
  refs _ _ _ = []

deriving via (WrapAeson mut imm Int) instance Content mut imm Int

instance Refs mut imm (mut a) where
  refs fm _ m = [fm m]

instance Content mut imm (mut a) where
  encode fm _ = fm
  decode _ _ fm _ = fm

instance Refs mut imm (imm a) where
  refs _ fi m = [fi m]

instance Content mut imm (imm a) where
  encode _ fi = fi
  decode _ _ _ fi = fi

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
