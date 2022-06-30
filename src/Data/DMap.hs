{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The design of this module is inspired by https://hackage.haskell.org/package/HMap.
-- This module is more obviously unsound that HMap is, but we more clearly outline the conditions for safety, and put the onus of maintaining them on the user.
module Data.DMap
  ( DMap,
    DKey (unDKey),
    unsafeMkDKey,
    lookup,
    insert,
    member,
    member',
    delete,
    delete',
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Bool, Eq, Maybe, Monoid, Ord, Semigroup, (<$>))

-- | Caches are backed by a HashMap.
-- Since our keys are wide integers, HashMap should give us a close and fast approximation of a wide integer IntMap.
newtype DMap k = DMap (HashMap k Any)
  deriving newtype (Monoid, Semigroup)

-- | Given the uniqueness constraints/requirements imposed by unsafeMkDKey,
-- a DKey should act as a witness that this @k@ is unqiuely associated with type @a@
newtype DKey k a = DKey {unDKey :: k}
  deriving (Eq, Ord)

-- | This is safe iff the key is not used elsewhere to create a DKey of a different type.
-- In our case, DKeys are come from either UUIDs, or content hashes.
--
-- UUIDs are unique per type if we immediately create a single, monomorphic DKey, e.g. using Typeable.
-- This is what HMap does, but HMap doesn't prohibit accidentally making polymorphic keys, which are unsound.
--
-- Content hashes are unique per type if we salt them with their types.
unsafeMkDKey :: (Eq k, Hashable k) => k -> DKey k a
unsafeMkDKey = DKey

lookup :: (Eq k, Hashable k) => DKey k a -> DMap k -> Maybe a
lookup (DKey ix) (DMap c) = unsafeCoerce <$> HashMap.lookup ix c

member :: (Eq k, Hashable k) => DKey k a -> DMap k -> Bool
member (DKey ix) = member' ix

member' :: (Eq k, Hashable k) => k -> DMap k -> Bool
member' ix (DMap c) = HashMap.member ix c

insert :: (Eq k, Hashable k) => a -> DKey k a -> DMap k -> DMap k
insert a (DKey ix) (DMap c) = DMap (HashMap.insert ix (unsafeCoerce a) c)

delete :: (Eq k, Hashable k) => DKey k a -> DMap k -> DMap k
delete (DKey ix) = delete' ix

delete' :: (Eq k, Hashable k) => k -> DMap k -> DMap k
delete' ix (DMap c) = DMap (HashMap.delete ix c)