{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Var where

import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.DMap (DKey (..), unsafeMkDKey)
import Data.Hashable (Hashable)
import Data.LargeWords (Word128 (..))
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.UUID as UUID

newtype UUID = UUID Word128
  deriving newtype (Eq, Ord, Hashable)

instance Show UUID where
  show u = show $ toUUID u

instance Read UUID where
  readsPrec d x =
    fmap (first fromUUID) (readsPrec d x)

instance Serialize UUID where
  put (UUID (Word128 a b)) = Serialize.put a <> Serialize.put b
  get = UUID <$> (Word128 <$> Serialize.get <*> Serialize.get)

newtype Var a = Var {unVar :: DKey UUID (TVar a)}
  deriving (Eq, Ord)

instance Show (Var a) where
  show (Var key) = show (unDKey key)

fakeVar :: Var a
fakeVar = Var $ unsafeMkDKey $ UUID $ Word128 0 0

instance Serialize (Var a) where
  put (Var key) = Serialize.put $ unDKey key
  get = Var . unsafeMkDKey <$> Serialize.get

instance FromJSON (Var a) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    case UUID.fromText txt of
      Nothing -> fail (show txt <> " isn't a valid UUID")
      Just a -> pure $ Var $ unsafeMkDKey $ fromUUID a

toUUID :: UUID -> UUID.UUID
toUUID (UUID (Word128 a b)) = UUID.fromWords64 a b

fromUUID :: UUID.UUID -> UUID
fromUUID u = UUID $ Word128 a b
  where
    (a, b) = UUID.toWords64 u

varUuid :: Var a -> UUID.UUID
varUuid = toUUID . varUuid'

varUuid' :: Var a -> UUID
varUuid' = unDKey . unVar

instance ToJSON (Var a) where
  toJSON (Var key) = Aeson.String (UUID.toText (toUUID (unDKey key)))
