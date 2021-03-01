{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Content
  ( SHA256 (..),
    Address (..),
    Content (..),
    GContent (..),
  )
where

import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Sequence (Seq)
import Data.Serialize
import Data.Set (Set)
import Data.Tree (Tree)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural

class Serialize a => Content a where
  references :: (forall b. Address b -> ref) -> a -> [ref]
  default references ::
    (Generic a, GContent (Rep a)) =>
    (forall b. Address b -> ref) ->
    a ->
    [ref]
  references f a = greferences f (from a)

-- TODO: Short bytestrings could lead to better memory performance here
newtype SHA256 = SHA256 {unSHA256 :: BS.ByteString}
  deriving (Eq, Ord, Serialize)

newtype Address a = Address {forget :: SHA256}
  deriving (Eq, Ord, Serialize)

class GContent a where greferences :: (forall b. Address b -> ref) -> a x -> [ref]

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

fromFoldable :: (Content a, Foldable t) => (forall b. Address b -> ref) -> t a -> [ref]
fromFoldable f = foldMap (references f)

instance Content r => GContent (K1 m r) where
  greferences f (K1 r) = references f r

instance Content (Address a) where references f = pure . f

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

instance Content BS.ByteString where references _ = mempty

instance Content BL.ByteString where references _ = mempty

instance Content IntSet where references _ = mempty

instance Content Any where references _ = mempty

instance Content All where references _ = mempty

instance Content a => Content [a]

instance Content a => Content (Maybe a)

instance Content e => Content (IntMap e) where references = fromFoldable

instance Content e => Content (Seq e) where references = fromFoldable

instance (Ord a, Content a) => Content (Set a) where references = fromFoldable

instance Content e => Content (Tree e) where references = fromFoldable

instance Content a => Content (Product a)

instance Content a => Content (Dual a)

instance Content a => Content (Sum a)

instance Content a => Content (First a)

instance Content a => Content (Last a)

instance (Content a, Content b) => Content (Either a b)

instance (Content k, Ord k, Content e) => Content (Map k e) where references f = fromFoldable f . M.toList

instance (Content a, Content b) => Content (a, b)

instance (Content a, Content b, Content c) => Content (a, b, c)

instance (Content a, Content b, Content c, Content d) => Content (a, b, c, d)
