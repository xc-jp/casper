{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Var where

import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.DMap (DKey, unDKey, unsafeMkDKey)
import Data.Hashable (Hashable)
import Data.Kind
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import Data.Void (Void)
import System.Mem.Weak (Weak)

-- TODO What is this wrapper for?
newtype UUID = UUID {unUUID :: UUID.UUID}
  deriving newtype (Eq, Ord, Hashable, Read, Show)

instance Serialize UUID where
  put = Serialize.put . UUID.toWords64 . unUUID
  get = UUID . uncurry UUID.fromWords64 <$> Serialize.get

newtype Var a (s :: k) = Var {unVar :: DKey UUID a}
  deriving (Eq, Ord)

instance Show (Var a s) where show = show . unDKey . unVar

instance Serialize (Var a s) where
  put = Serialize.put . unDKey . unVar
  get = Var . unsafeMkDKey <$> Serialize.get

instance ToJSON (Var a s) where
  toJSON var = Aeson.String (UUID.toText (unUUID $ varUuid var))

instance FromJSON (Var a s) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    case UUID.fromText txt of
      Nothing -> fail (show txt <> " isn't a valid UUID")
      Just a -> pure $ Var $ unsafeMkDKey $ UUID a

varUuid :: Var a s -> UUID
varUuid = unDKey . unVar

rescopeVar :: Var a s -> Var a t
rescopeVar = coerce

nextVar :: IO (Var a s)
nextVar = UUID.nextUUID >>= maybe nextVar (pure . Var . unsafeMkDKey . UUID)
