{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ref where

import DMap (DKey (..), unsafeMkDKey)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Text.Encoding as Text
import LargeWords (Word256 (..))

newtype Ref a s = Ref (DKey SHA a)
  deriving (Eq, Ord)

instance Show (Ref a s) where
  show (Ref key) = show (unDKey key)

fakeRef :: Ref a s
fakeRef = Ref $ unsafeMkDKey $ SHA (Word256 0 0 0 0)

instance Serialize (Ref a s) where
  get = Ref . unsafeMkDKey <$> Serialize.get
  put (Ref key) = Serialize.put $ unDKey key

instance ToJSON (Ref a s) where
  toJSON ref =
    Aeson.String $
      Text.decodeLatin1 $
        Base64.encode $
          Serialize.runPut $
            Serialize.put ref

instance FromJSON (Ref a s) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    let digest = Text.encodeUtf8 txt
    case Base64.decode digest of
      Left err -> fail err
      Right bytes -> case Serialize.runGet Serialize.get bytes of
        Left err -> fail err
        Right x -> pure x

newtype SHA = SHA Word256
  deriving newtype (Eq, Ord, Hashable)

instance Show SHA where
  show sha = Char8.unpack $ Base64.encode (Serialize.runPut $ Serialize.put sha)

instance Read SHA where
  readsPrec _ str =
    let nbytes = 32
        nchars = 4 * ceiling (nbytes / 3 :: Double)
     in case Base64.decode (Char8.pack (take nchars str)) of
          Left _ -> []
          Right b -> case Serialize.runGet Serialize.get b of
            Left _ -> []
            Right sha -> [(sha, drop nchars str)]

instance Serialize SHA where
  get = SHA <$> (Word256 <$> Serialize.get <*> Serialize.get <*> Serialize.get <*> Serialize.get)
  put (SHA (Word256 a b c d)) = Serialize.put a <> Serialize.put b <> Serialize.put c <> Serialize.put d
