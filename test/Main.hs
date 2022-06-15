{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

import qualified Casper
import Casper (CasperT)

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import System.IO.Temp (withTempDirectory)

import Test.Hspec (Spec, it, example)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

----------------------------------------------------------------------------------------------------

type TestString = ByteString

newtype TrivialTestType s = TrivialTestType [Casper.Ref TestString s]
  deriving stock (Generic)
  deriving (Casper.Content)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via Casper.WrapAeson (TrivialTestType s)

initTrivial :: TrivialTestType s
initTrivial = TrivialTestType []

simpleTests :: Spec
simpleTests =
  let hello = "Hello, world!" :: TestString in
  it ("opens a new content addressable store, stores a single value " <> show hello) $
  example $
  withTempDirectory "." "casper-test-store.XXXXXXXX" $ \ testStorePath ->
  Casper.openStore testStorePath initTrivial $ \ refList0 -> do
    rootVar <- Casper.transact $ Casper.newVar refList0
    Casper.transact $ do
      ref <- Casper.newRef hello
      (TrivialTestType refList) <- Casper.readVar rootVar
      Casper.writeVar rootVar $ TrivialTestType $ ref:refList

main :: IO ()
main =
  testGroup "Casper" <$>
  sequence
  [ testGroup "Simple Tests" <$> testSpecs simpleTests
  ] >>=
  defaultMain
