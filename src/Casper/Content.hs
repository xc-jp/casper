{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Casper.Content where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.LargeWords (Word128, Word256)
import qualified Data.Map as Map
import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Ratio (Ratio)
import qualified Data.Ratio as Ratio
import Data.Ref (Ref)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Tree (Tree)
import Data.Var (Var)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1, V1, type (:*:) (..), type (:+:) (..))

-- | This provides the 'refs' method for traversing over a data type and extracting all of the
-- direct children for some content. 'Var's and 'Ref's that are not included in this traveral may be
-- garbage collected.
--
-- We don't provide an instance for 'ByteString' because the default
-- 'Serialize' instance prefixes the payload with the length. This is likely
-- not what you want if you want to store some binary file that is to be read
-- by other processes. By not providing the 'Content' instance it's less likely
-- that you'll use this serialization method by mistake since you can't use
-- 'writeVar' and 'newRef'.
--
-- 'Serialize' is not a superclass of 'Content' because there's no instance for
-- 'Serialize' 'Void'.
class Content a where
  refs ::
    (forall r. Var r -> ref) ->
    (forall r. Ref r -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent (Rep a)) =>
    (forall r. Var r -> ref) ->
    (forall r. Ref r -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

class GContent a where
  grefs ::
    (forall r. Var r -> ref) ->
    (forall r. Ref r -> ref) ->
    (a x -> [ref])

noRefs :: (forall r. Var r -> ref) -> (forall r. Ref r -> ref) -> a -> [ref]
noRefs _ _ _ = []

instance Content a => GContent (K1 c a) where grefs fr fc (K1 a) = refs fr fc a

instance GContent a => GContent (M1 i c a) where grefs fr fc (M1 a) = grefs fr fc a

instance (GContent a, GContent b) => GContent (a :*: b) where grefs fr fc (a :*: b) = grefs fr fc a <> grefs fr fc b

instance (GContent a, GContent b) => GContent (a :+: b) where
  grefs fr fc (L1 a) = grefs fr fc a
  grefs fr fc (R1 b) = grefs fr fc b

foldRefs :: (Foldable t, Content a) => (forall r. Var r -> ref) -> (forall r. Ref r -> ref) -> t a -> [ref]
foldRefs fv fr = foldMap (refs fv fr)

instance GContent U1 where grefs = noRefs

instance GContent V1 where grefs = noRefs

instance Content Void

instance (Content a) => Content (Ref a) where refs _ fc c = pure $ fc c

instance (Content a) => Content (Var a) where refs fr _ r = pure $ fr r

instance Content a => Content [a]

instance Content a => Content (Maybe a)

instance (Ord k, Content k, Content a) => Content (Map.Map k a) where
  refs fv fr = Map.foldMapWithKey (\k a -> refs fv fr k <> refs fv fr a)

instance (Ord k, Content k) => Content (Set.Set k) where refs = foldRefs

instance Content a => Content (IntMap a) where refs = foldRefs

instance Content a => Content (Tree a) where refs = foldRefs

instance Content a => Content (Seq a) where refs = foldRefs

instance Content IntSet where refs = noRefs

instance Content Bool where refs = noRefs

instance Content Char where refs = noRefs

instance Content Double where refs = noRefs

instance Content Float where refs = noRefs

instance Content Int where refs = noRefs

instance Content Int8 where refs = noRefs

instance Content Int16 where refs = noRefs

instance Content Int32 where refs = noRefs

instance Content Int64 where refs = noRefs

instance Content Integer where refs = noRefs

instance Content Word where refs = noRefs

instance Content Word8 where refs = noRefs

instance Content Word16 where refs = noRefs

instance Content Word32 where refs = noRefs

instance Content Word64 where refs = noRefs

instance Content Word128 where refs = noRefs

instance Content Word256 where refs = noRefs

instance (Integral a, Content a) => Content (Ratio a) where
  refs fv fr a = refs fv fr (Ratio.numerator a) <> refs fv fr (Ratio.denominator a)

instance Content () where refs = noRefs

instance Content All where refs = noRefs

instance Content Any where refs = noRefs

instance Content a => Content (First a)

instance Content a => Content (Last a)

instance Content a => Content (Dual a)

instance Content a => Content (Sum a)

instance Content a => Content (Product a)

instance (Content a, Content b) => Content (Either a b)

instance (Content a, Content b) => Content (a, b)

instance (Content a, Content b, Content c) => Content (a, b, c)

instance (Content a, Content b, Content c, Content d) => Content (a, b, c, d)

instance (Content a, Content b, Content c, Content d, Content e) => Content (a, b, c, d, e)

instance (Content a, Content b, Content c, Content d, Content e, Content f) => Content (a, b, c, d, e, f)

instance (Content a, Content b, Content c, Content d, Content e, Content f, Content g) => Content (a, b, c, d, e, f, g)

instance (Content a, Content b, Content c, Content d, Content e, Content f, Content g, Content h) => Content (a, b, c, d, e, f, g, h) where
  refs fv fr (a, b, c, d, e, f, g, h) =
    mconcat
      [ refs fv fr a,
        refs fv fr b,
        refs fv fr c,
        refs fv fr d,
        refs fv fr e,
        refs fv fr f,
        refs fv fr g,
        refs fv fr h
      ]

instance (Content a, Content b, Content c, Content d, Content e, Content f, Content g, Content h, Content i) => Content (a, b, c, d, e, f, g, h, i) where
  refs fv fr (a, b, c, d, e, f, g, h, i) =
    mconcat
      [ refs fv fr a,
        refs fv fr b,
        refs fv fr c,
        refs fv fr d,
        refs fv fr e,
        refs fv fr f,
        refs fv fr g,
        refs fv fr h,
        refs fv fr i
      ]

instance (Content a, Content b, Content c, Content d, Content d, Content e, Content f, Content g, Content h, Content i, Content j) => Content (a, b, c, d, e, f, g, h, i, j) where
  refs fv fr (a, b, c, d, e, f, g, h, i, j) =
    mconcat
      [ refs fv fr a,
        refs fv fr b,
        refs fv fr c,
        refs fv fr d,
        refs fv fr e,
        refs fv fr f,
        refs fv fr g,
        refs fv fr h,
        refs fv fr i,
        refs fv fr j
      ]
