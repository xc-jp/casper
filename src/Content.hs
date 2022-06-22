{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Content where

import GHC.Generics
import Ref (Ref)
import Var (Var)

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
class Content a where
  refs ::
    (forall s r. Var r s -> ref) ->
    (forall s r. Ref r s -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent (Rep a)) =>
    (forall s r. Var r s -> ref) ->
    (forall s r. Ref r s -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

-- TODO: should 'Content' have 'Serialize' as a superclass constraint again?

class GContent a where
  grefs ::
    (forall s r. Var r s -> ref) ->
    (forall s r. Ref r s -> ref) ->
    (a x -> [ref])

instance Content a => GContent (K1 c a) where grefs fr fc (K1 a) = refs fr fc a

instance GContent a => GContent (M1 i c a) where grefs fr fc (M1 a) = grefs fr fc a

instance (GContent a, GContent b) => GContent (a :*: b) where grefs fr fc (a :*: b) = grefs fr fc a <> grefs fr fc b

instance (GContent a, GContent b) => GContent (a :+: b) where
  grefs fr fc (L1 a) = grefs fr fc a
  grefs fr fc (R1 b) = grefs fr fc b

instance GContent U1 where grefs _ _ _ = []

instance GContent V1 where grefs _ _ _ = []

instance Content (Ref a s) where refs _ fc c = pure $ fc c

instance Content (Var a s) where refs fr _ r = pure $ fr r

instance Content a => Content [a]

instance Content a => Content (Maybe a)

instance Content Int where refs _ _ _ = []
