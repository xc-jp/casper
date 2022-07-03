{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

module Data.Ref where

import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import Data.DMap (DKey, unDKey, unsafeMkDKey)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Text.Encoding as Text
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Mem.Weak (Weak)

newtype Ref a (s :: k) = Ref {unRef :: DKey SHA a}
  deriving newtype (Eq, Ord)

instance Show (Ref a s) where
  show = show . unDKey . unRef

instance Serialize (Ref a s) where
  get = Ref . unsafeMkDKey <$> Serialize.get
  put = Serialize.put . unDKey . unRef

instance ToJSON (Ref a s) where
  toJSON ref =
    Aeson.String $
      Text.decodeLatin1 $
        Base64.encode $
          Serialize.encode ref

instance FromJSON (Ref a s) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    let digest = Text.encodeUtf8 txt
    case Base64.decode digest >>= Serialize.decode of
      Left err -> fail err
      Right x -> pure x

data SHA = SHA Word64 Word64 Word64 Word64
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable, Serialize)

instance Show SHA where
  show = Char8.unpack . Base64.encode . Serialize.encode

instance Read SHA where
  readsPrec _ str =
    let nbytes = 32
        nchars = 4 * ceiling (nbytes / 3 :: Double)
        bytes = Char8.pack (take nchars str)
     in case Base64.decode bytes >>= Serialize.decode of
          Left _ -> []
          Right sha -> [(sha, drop nchars str)]
