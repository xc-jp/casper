{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Casper.Resource (Loc (..), Resource (..), GResource (..), JSONResource) where

import Casper.Content (Ref, SHA256)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
  ( Dual (Dual),
    First (First),
    Last (Last),
    Product (Product),
    Sum (Sum),
  )
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Tree)
import Data.UUID (UUID)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural (Natural)

newtype Loc s a = Loc {forgetLocation :: UUID}
  deriving (Show, Eq, Ord)

instance Aeson.ToJSONKey (Loc s a)

instance Aeson.FromJSONKey (Loc s a)

instance Aeson.ToJSON (Loc s a) where
  toJSON (Loc a) = Aeson.toJSON a

instance Aeson.FromJSON (Loc s a) where
  parseJSON v = Loc <$> Aeson.parseJSON v

class Resource a where
  resourceReferences ::
    (forall s b. Loc s b -> loc) ->
    (forall s b. Ref s b -> ref) ->
    a ->
    ([loc], [ref])
  encodeResource :: a -> ByteString
  decodeResource :: ByteString -> Either String a

  default resourceReferences ::
    (Generic a, GResource (Rep a)) =>
    (forall s b. Loc s b -> loc) ->
    (forall s b. Ref s b -> ref) ->
    a ->
    ([loc], [ref])
  resourceReferences f g a = gresourceReferences f g (from a)
  default encodeResource :: (Aeson.ToJSON a) => a -> ByteString
  encodeResource = BL.toStrict . Aeson.encode
  default decodeResource :: Aeson.FromJSON a => ByteString -> Either String a
  decodeResource = Aeson.eitherDecodeStrict'

instance Resource (Loc s a) where resourceReferences f _ l = ([f l], [])

instance Resource (Ref s a) where resourceReferences _ g r = ([], [g r])

instance Resource SHA256 where resourceReferences _ _ = mempty

instance Resource Bool where resourceReferences _ _ = mempty

instance Resource Char where resourceReferences _ _ = mempty

instance Resource Double where resourceReferences _ _ = mempty

instance Resource Float where resourceReferences _ _ = mempty

instance Resource Int where resourceReferences _ _ = mempty

instance Resource Int8 where resourceReferences _ _ = mempty

instance Resource Int16 where resourceReferences _ _ = mempty

instance Resource Int32 where resourceReferences _ _ = mempty

instance Resource Int64 where resourceReferences _ _ = mempty

instance Resource Integer where resourceReferences _ _ = mempty

instance Resource Natural where resourceReferences _ _ = mempty

instance Resource Ordering where resourceReferences _ _ = mempty

instance Resource Word where resourceReferences _ _ = mempty

instance Resource Word8 where resourceReferences _ _ = mempty

instance Resource Word16 where resourceReferences _ _ = mempty

instance Resource Word32 where resourceReferences _ _ = mempty

instance Resource Word64 where resourceReferences _ _ = mempty

instance Resource () where resourceReferences _ _ = mempty

instance Resource Void where resourceReferences _ _ = mempty

instance Resource IntSet where resourceReferences _ _ = mempty

class GResource a where
  gresourceReferences ::
    (forall s b. Loc s b -> loc) ->
    (forall s b. Ref s b -> ref) ->
    a x ->
    ([loc], [ref])

instance (GResource l, GResource r) => GResource (l :*: r) where
  gresourceReferences f g (l :*: r) = gresourceReferences f g l <> gresourceReferences f g r

instance (GResource l, GResource r) => GResource (l :+: r) where
  gresourceReferences f g (L1 l) = gresourceReferences f g l
  gresourceReferences f g (R1 r) = gresourceReferences f g r

instance GResource V1 where
  gresourceReferences _ _ _ = ([], [])

instance (GResource x) => GResource (M1 m i x) where
  gresourceReferences f g (M1 x) = gresourceReferences f g x

instance GResource U1 where
  gresourceReferences _ _ _ = ([], [])

instance Resource r => GResource (K1 m r) where
  gresourceReferences f g (K1 r) = resourceReferences f g r

type JSONResource a = (Aeson.FromJSON a, Aeson.ToJSON a, Resource a)

deriving instance JSONResource a => Resource (Product a)

deriving instance JSONResource a => Resource (Dual a)

deriving instance JSONResource a => Resource (Sum a)

deriving instance JSONResource a => Resource (First a)

deriving instance JSONResource a => Resource (Last a)

instance (JSONResource a) => Resource [a]

instance JSONResource a => Resource (Maybe a)

instance (JSONResource a, JSONResource b) => Resource (Either a b)

instance (JSONResource a, JSONResource b) => Resource (a, b)

instance (JSONResource a, JSONResource b, JSONResource c) => Resource (a, b, c)

instance (JSONResource a, JSONResource b, JSONResource c, JSONResource d) => Resource (a, b, c, d)

fromFoldable :: (Resource a, Foldable t) => (forall s b. Loc s b -> loc) -> (forall s b. Ref s b -> ref) -> t a -> ([loc], [ref])
fromFoldable f g = foldMap (resourceReferences f g)

instance (JSONResource k, Aeson.FromJSONKey k, Aeson.ToJSONKey k, Ord k, JSONResource e) => Resource (Map k e) where
  resourceReferences f g = fromFoldable f g . M.toList

instance JSONResource e => Resource (IntMap e) where resourceReferences = fromFoldable

instance JSONResource e => Resource (Seq e) where resourceReferences = fromFoldable

instance (Ord a, JSONResource a) => Resource (Set a) where resourceReferences = fromFoldable

instance JSONResource e => Resource (Tree e) where resourceReferences = fromFoldable
