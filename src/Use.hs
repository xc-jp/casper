{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Use where

import Casper
import Control.Applicative ((<|>))
import Control.Lens (at, (?~))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.String (fromString)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics

data Root s = Root (Var (Foo s) s) (Var (Foo s) s)
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via WrapAeson (Root s)

data Foo s = Foo
  { mi :: Var Int s,
    _mli :: Var [Int] s,
    lmi :: [Var Int s],
    rec :: Var (Foo s) s,
    recs :: Var [Foo s] s,
    val :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via (MetaAeson (RemoveUnderscores (Foo s)))
  deriving (Typeable)
  deriving (JSONMeta)

deriving instance Content (Foo s)

-- deriving via (GenericRescope (Foo x)) instance Rescope (Foo s) (Foo t)
deriving instance Rescope (Foo s) (Foo t)

-- TODO what the fuck how do we prevent this
whatTheFuckHowDoWePreventThis :: Var Int a -> Var Int b
whatTheFuckHowDoWePreventThis = coerce

fooType :: String
fooType = show $ typeRep (Proxy :: Proxy (Foo ()))

-- | Modify the To/FromJSON instances to output a "meta" field that is embedded
-- in the object if the original thing serializes to an object and constructs
-- the object {"meta": metadata, "value": original} otherwise.
--
-- useful with 'deriving via (MetaAeson YourDataType)'
newtype MetaAeson a = MetaAeson {unMetaAeson :: a}

newtype RemoveUnderscores a = RemoveUnderscores {unRemoveUnderscores :: a}
  deriving newtype (JSONMeta)

removeUndercores :: Options
removeUndercores =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \s -> fromMaybe s (stripPrefix "_" s),
      Aeson.constructorTagModifier = \s -> fromMaybe s (stripPrefix "_" s)
    }

instance (GFromJSON Zero (Rep a), Generic a) => Aeson.FromJSON (RemoveUnderscores a) where
  parseJSON = fmap RemoveUnderscores . Aeson.genericParseJSON removeUndercores

instance (GToJSON Zero (Rep a), Generic a) => Aeson.ToJSON (RemoveUnderscores a) where
  toJSON (RemoveUnderscores a) = Aeson.genericToJSON removeUndercores a

instance (Aeson.FromJSON a, Aeson.ToJSON a, JSONMeta a) => Serialize (MetaAeson a) where
  put = Serialize.put . Aeson.encode
  get = Serialize.get >>= either fail pure . Aeson.eitherDecodeStrict'

class JSONMeta a where
  jsonMeta :: a -> Aeson.Value
  default jsonMeta :: (Generic a, GToJSONMeta (Rep a)) => a -> Aeson.Value
  jsonMeta a = gjsonMeta (from a)

class GToJSONMeta a where
  gjsonMeta :: a x -> Aeson.Value

instance (GToJSONMeta l, GToJSONMeta r) => GToJSONMeta (l :+: r) where
  gjsonMeta (L1 a) = gjsonMeta a
  gjsonMeta (R1 a) = gjsonMeta a

instance (GToJSONMeta l, GToJSONMeta r) => GToJSONMeta (l :*: r) where
  gjsonMeta (a :*: b) = Aeson.toJSON [gjsonMeta a, gjsonMeta b]

instance (Constructor c, GToJSONMeta a) => GToJSONMeta (C1 c a) where
  gjsonMeta c@(M1 a) = Aeson.object [("constructor", Aeson.String (fromString $ conName c)), ("meta", gjsonMeta a)]

instance (Selector c, GToJSONMeta a) => GToJSONMeta (S1 c a) where
  gjsonMeta c@(M1 a) = Aeson.object [("field", Aeson.String (fromString $ selName c)), ("meta", gjsonMeta a)]

instance (Datatype c, GToJSONMeta a) => GToJSONMeta (D1 c a) where
  gjsonMeta c@(M1 a) = Aeson.object [("type", Aeson.String (fromString $ datatypeName c)), ("meta", gjsonMeta a)]

instance {-# OVERLAPPABLE #-} GToJSONMeta a => GToJSONMeta (M1 i c a) where
  gjsonMeta (M1 a) = gjsonMeta a

instance GToJSONMeta (K1 i a) where
  gjsonMeta _ = Aeson.Null

instance GToJSONMeta U1 where
  gjsonMeta _ = Aeson.Null

instance GToJSONMeta V1 where
  gjsonMeta _ = Aeson.Null

instance (Aeson.ToJSON a, JSONMeta a) => Aeson.ToJSON (MetaAeson a) where
  toJSON (MetaAeson a) = case Aeson.toJSON a of
    Aeson.Object o -> Aeson.Object $ (at "meta" ?~ jsonMeta a) o
    v -> Aeson.object [("value", v), ("meta", jsonMeta a)]

instance (Aeson.FromJSON a, JSONMeta a) => Aeson.FromJSON (MetaAeson a) where
  parseJSON value =
    MetaAeson <$> (parseJSON value >>= \o -> o .: "value" <|> parseJSON (Aeson.Object o))

-- >>> import qualified Data.Aeson as Aeson
-- >>> Serialize.runPut (Serialize.put exampleFoo)
-- "\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL{\"mi\":\"00000000-0000-0000-0000-000000000000\",\"val\":2,\"mli\":\"00000000-0000-0000-0000-000000000000\",\"meta\":{\"meta\":{\"constructor\":\"Foo\",\"meta\":[[{\"meta\":null,\"field\":\"mi\"},[{\"meta\":null,\"field\":\"_mli\"},{\"meta\":null,\"field\":\"lmi\"}]],[{\"meta\":null,\"field\":\"rec\"},[{\"meta\":null,\"field\":\"recs\"},{\"meta\":null,\"field\":\"val\"}]]]},\"type\":\"Foo\"},\"recs\":\"00000000-0000-0000-0000-000000000000\",\"rec\":\"00000000-0000-0000-0000-000000000000\",\"lmi\":[\"00000000-0000-0000-0000-000000000000\",\"00000000-0000-0000-0000-000000000000\"]}"
exampleFoo :: Foo s
exampleFoo = Foo fakeVar fakeVar [fakeVar, fakeVar] fakeVar fakeVar 2

someFunc :: IO Int
someFunc =
  loadStore "/dev/null" $ \(Root l _) -> do
    transact $ do
      foo1 <- readVar l
      foo2 <- readVar (rec foo1)
      writeVar (mi foo2) 0
    borrow (readVar l >>= readVar . rec) $ \l' -> do
      transact $ readVar (mi l')
