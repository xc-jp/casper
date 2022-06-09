{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

import qualified Casper

import Control.Monad (unless)

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import System.Directory
import System.FilePath

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, testSpecs, it, example)

----------------------------------------------------------------------------------------------------

type TestString = ByteString

newtype TrivialTestType s = TrivialTestType [Casper.Ref TestString s]
  deriving stock (Generic)
  deriving (Casper.Content)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via Casper.WrapAeson (TrivialTestType s)

initTrivial :: TrivialTestType s
initTrivial = TrivialTestType []

testStorePath :: FilePath
testStorePath = "./casper-test-store"

simpleTests :: Spec
simpleTests = do
  let hello = "Hello, world!" :: TestString
  it ("opens a new content addressable store, stores a single value " <> show hello) $
    example $ do
      (root, store) <- Casper.openStore testStorePath initTrivial $ \ root -> do
        root <- Casper.transact $ Casper.newVar root
        store <- Casper.getStore
        pure (root, store)
      Casper.runCasperT store $
        Casper.transact $ do
          ref <- Casper.newRef hello
          (TrivialTestType refList) <- Casper.readVar root
          Casper.writeVar root $ TrivialTestType $ ref:refList

main :: IO ()
main =
  testGroup "Casper" <$>
  sequence
  [ testGroup "Simple Tests" <$> testSpecs simpleTests
  ] >>=
  defaultMain
