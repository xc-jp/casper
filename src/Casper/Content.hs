{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Casper.Content
  ( SHA256 (..),
    Ref (..),
    Content (..),
    GContent (..),
    JSONContent,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as BChar
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BShort
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
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural

class Content a where
  references :: (forall s b. Ref s b -> ref) -> a -> [ref]
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

newtype Ref s a = Ref {forget :: SHA256}
  deriving (Eq, Ord)

instance Show (Ref s a) where
  show (Ref sha) = show sha

-- | A 'Ref' is storable as JSON. You should take care not to use the
-- 'FromJSON' instance on anything that doesn't come from the Casper store
-- because those references are not guaranteed to be valid.
instance Aeson.ToJSON (Ref s a) where
  toJSON (Ref s) = Aeson.toJSON s

instance Aeson.FromJSON (Ref s a) where
  parseJSON v = Ref <$> Aeson.parseJSON v

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

instance Content (Ref s a) where references f = pure . f

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
