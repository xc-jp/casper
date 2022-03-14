{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Casper.Content
  ( SHA256 (..),
    Ref (..),
    Content (..),
    GContent (..),
    JSONContent,
    TraversableContent (..),
    GTraversableContent (..),
  )
where

import Control.Applicative (liftA2)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as BChar
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BShort
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (All (..), Any (..), Dual (..), First (..), Last (..), Product (..), Sum (..))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String (fromString)
import Data.Tree (Tree)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural

class Content a where
  references :: (forall s r. Ref s r -> ref) -> a -> [ref]
  encodeContent :: a -> BS.ByteString
  decodeContent :: BS.ByteString -> Either String a

  default references ::
    (Generic a, GContent (Rep a)) =>
    (forall s b. Ref s b -> ref) ->
    a ->
    [ref]
  references f a = greferences f (from a)
  default encodeContent :: (Aeson.ToJSON a) => a -> BS.ByteString
  encodeContent = BL.toStrict . Aeson.encode
  default decodeContent :: Aeson.FromJSON a => BS.ByteString -> Either String a
  decodeContent = Aeson.eitherDecodeStrict'

newtype SHA256 = SHA256 {unSHA256 :: BShort.ShortByteString}
  deriving (Eq, Ord)

instance Show SHA256 where
  show (SHA256 a) = BChar.unpack $ Base64.encode (BShort.fromShort a)

instance Read SHA256 where
  readsPrec _ s =
    case Base64.decode (BChar.pack s) of
      Left _ -> []
      Right x -> pure (SHA256 (BShort.toShort x), "")

newtype Ref (s :: *) (a :: *) = Ref {forget :: SHA256}
  deriving (Eq, Ord)

instance Show (Ref s a) where
  show (Ref sha) = show sha

-- | A 'Ref' is storable as JSON. You should take care not to use the
-- 'FromJSON' instance on anything that doesn't come from the Casper store
-- because those references are not guaranteed to be valid.
instance Aeson.ToJSON (Ref s a) where
  toJSON (Ref s) = Aeson.toJSON s

instance Aeson.ToJSON1 (Ref s) where
  liftToJSON _ _ = Aeson.toJSON . forget
  liftToEncoding _ _ = Aeson.toEncoding . forget

instance Aeson.FromJSON (Ref s a) where
  parseJSON v = Ref <$> Aeson.parseJSON v

instance Aeson.FromJSON1 (Ref s) where
  liftParseJSON _ _ = fmap Ref . Aeson.parseJSON

-- | A 'SHA256' is serialized to a Base64 JSON string with the URL safe
-- alphabet https://tools.ietf.org/rfc/rfc4648#section-5.
instance Aeson.ToJSON SHA256 where
  toJSON (SHA256 s) =
    Aeson.String
      . fromString
      . BChar.unpack
      . Base64.encode
      . BShort.fromShort
      $ s

-- | A 'SHA256' is deserialized from a Base64 JSON string with the URL safe
-- alphabet https://tools.ietf.org/rfc/rfc4648#section-5.
instance Aeson.FromJSON SHA256 where
  parseJSON v = do
    s <- Aeson.parseJSON v
    case Base64.decode (BChar.pack s) of
      Left err -> fail err
      Right x -> pure (SHA256 (BShort.toShort x))

class GContent a where greferences :: (forall s b. Ref s b -> ref) -> a x -> [ref]

instance (GContent l, GContent r) => GContent (l :*: r) where
  greferences f (l :*: r) = greferences f l <> greferences f r

instance (GContent l, GContent r) => GContent (l :+: r) where
  greferences f (L1 l) = greferences f l
  greferences f (R1 r) = greferences f r

instance GContent V1 where
  greferences _ _ = []

instance (GContent x) => GContent (M1 m i x) where
  greferences f (M1 x) = greferences f x

instance GContent U1 where
  greferences _ _ = []

fromFoldable :: (Content a, Foldable t) => (forall s b. Ref s b -> ref) -> t a -> [ref]
fromFoldable f = foldMap (references f)

instance Content r => GContent (K1 m r) where
  greferences f (K1 r) = references f r

instance Content (Ref s a) where references f x = pure (f x)

instance Content SHA256 where references _ = mempty

instance Content Bool where references _ = mempty

instance Content Char where references _ = mempty

instance Content Double where references _ = mempty

instance Content Float where references _ = mempty

instance Content Int where references _ = mempty

instance Content Int8 where references _ = mempty

instance Content Int16 where references _ = mempty

instance Content Int32 where references _ = mempty

instance Content Int64 where references _ = mempty

instance Content Integer where references _ = mempty

instance Content Natural where references _ = mempty

instance Content Ordering where references _ = mempty

instance Content Word where references _ = mempty

instance Content Word8 where references _ = mempty

instance Content Word16 where references _ = mempty

instance Content Word32 where references _ = mempty

instance Content Word64 where references _ = mempty

instance Content () where references _ = mempty

instance Content Void where references _ = mempty

instance Content IntSet where references _ = mempty

instance Content BS.ByteString where
  references _ = mempty
  encodeContent = id
  decodeContent = pure

instance Content BL.ByteString where
  references _ = mempty
  encodeContent = BL.toStrict
  decodeContent = pure . BL.fromStrict

deriving instance Content Any

deriving instance Content All

type JSONContent a = (Aeson.FromJSON a, Aeson.ToJSON a, Content a)

deriving instance JSONContent a => Content (Product a)

deriving instance JSONContent a => Content (Dual a)

deriving instance JSONContent a => Content (Sum a)

deriving instance JSONContent a => Content (First a)

deriving instance JSONContent a => Content (Last a)

instance (JSONContent a) => Content [a]

instance JSONContent a => Content (Maybe a)

instance (JSONContent a, JSONContent b) => Content (Either a b)

instance (JSONContent a, JSONContent b) => Content (a, b)

instance (JSONContent a, JSONContent b, JSONContent c) => Content (a, b, c)

instance (JSONContent a, JSONContent b, JSONContent c, JSONContent d) => Content (a, b, c, d)

instance (JSONContent k, Aeson.FromJSONKey k, Aeson.ToJSONKey k, Ord k, JSONContent e) => Content (Map k e) where
  references f = fromFoldable f . M.toList

instance JSONContent e => Content (IntMap e) where references = fromFoldable

instance JSONContent e => Content (Seq e) where references = fromFoldable

instance (Ord a, JSONContent a) => Content (Set a) where references = fromFoldable

instance JSONContent e => Content (Tree e) where references = fromFoldable

type HTraversal t f g =
  forall m.
  Applicative m =>
  ( forall a b.
    f a ->
    m (g b)
  ) ->
  t f ->
  m (t g)

class TraversableContent t f g where
  traverseRefs ::
    Applicative m =>
    ( forall a b.
      (Content a, Content b) =>
      f a ->
      m (g b)
    ) ->
    t f ->
    m (t g)
  default traverseRefs ::
    (forall h. Generic (t h), GTraversableContent (Rep (t f)) (Rep (t g)) f g) =>
    Applicative m =>
    ( forall a b.
      (Content a, Content b) =>
      f a ->
      m (g b)
    ) ->
    t f ->
    m (t g)
  traverseRefs f ts = to <$> gtraverseRefs f (from ts)

class GTraversableContent r r' f g where
  gtraverseRefs ::
    Applicative m =>
    ( forall a b.
      (Content a, Content b) =>
      f a ->
      m (g b)
    ) ->
    r x ->
    m (r' x)

instance (GTraversableContent l l' f g, GTraversableContent r r' f g) => GTraversableContent (l :*: r) (l' :*: r') f g where
  gtraverseRefs f (l :*: r) = liftA2 (:*:) (gtraverseRefs f l) (gtraverseRefs f r)

instance (GTraversableContent l l' f g, GTraversableContent r r' f g) => GTraversableContent (l :+: r) (l' :+: r') f g where
  gtraverseRefs f (L1 l) = L1 <$> gtraverseRefs f l
  gtraverseRefs f (R1 r) = R1 <$> gtraverseRefs f r

instance GTraversableContent V1 V1 f g where
  gtraverseRefs _ x = case x of {}

instance (GTraversableContent x x' f g) => GTraversableContent (M1 m i x) (M1 m i x') f g where
  gtraverseRefs f (M1 x) = M1 <$> gtraverseRefs f x

instance GTraversableContent U1 U1 f g where
  gtraverseRefs _ U1 = pure U1

{-
instance (TraversableContent x) => GTraversableContent (K1 i (f (x f))) (K1 i (g (x g))) f g where
  gtraverseRefs f (K1 x) = K1 <$> f x
-}

{-
instance GTraversableContent (K1 i (f x)) (K1 i (g x')) f g where
  gtraverseRefs f (K1 x) = K1 <$> f x
  -}

{-
instance Content (Datapoint (Ref s)) where
  references f (Datapoint r) = [f r]
  encodeContent = undefined
  decodeContent = undefined
  -}

-- instance TraversableContent Datapoint f g

-- Dataset Ref' -> Dataset (Ref s)

-- Ref s [Datapoint (Ref s)] -> Ref' [Datapoint Ref']

-- instance TraversableContent Dataset f g where
--   traverseRefs f (Dataset refs) = Dataset <$> f refs
