{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Var where

import Control.Concurrent.STM (TVar)
import DMap (DKey (..), unsafeMkDKey)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.UUID as UUID
import LargeWords (Word128 (..))

newtype UUID = UUID Word128
  deriving newtype (Eq, Ord, Hashable)

instance Show UUID where
  show (UUID w) = show $ toUUID w

instance Serialize UUID where
  put (UUID (Word128 a b)) = Serialize.put a <> Serialize.put b
  get = UUID <$> (Word128 <$> Serialize.get <*> Serialize.get)

newtype Var a s = Var {unVar :: DKey UUID (TVar a)}
  deriving (Eq, Ord)

instance Show (Var a s) where
  show (Var key) = show (unDKey key)

fakeVar :: Var a s
fakeVar = Var $ unsafeMkDKey $ UUID $ Word128 0 0

instance Serialize (Var a s) where
  put (Var key) = Serialize.put $ unDKey key
  get = Var . unsafeMkDKey <$> Serialize.get

instance FromJSON (Var a s) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    case UUID.fromText txt of
      Nothing -> fail (show txt <> " isn't a valid UUID")
      Just a -> pure $ Var $ unsafeMkDKey $ UUID (fromUUID a)

toUUID :: Word128 -> UUID.UUID
toUUID (Word128 a b) = UUID.fromWords64 a b

varUuid :: Var a s -> UUID
varUuid = unDKey . unVar

fromUUID :: UUID.UUID -> Word128
fromUUID u = Word128 a b
  where
    (a, b) = UUID.toWords64 u

instance ToJSON (Var a s) where
  toJSON (Var key) =
    let UUID w = unDKey key
     in Aeson.String (UUID.toText (toUUID w))
