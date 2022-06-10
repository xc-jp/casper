{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

import qualified Casper
import Casper (CasperT)

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

-- | This test suite does not use temporary directories, it stores everything in the same place
-- every time. The reason is so that if anything goes wrong we can inspect the state of the
-- database.
testStorePath :: FilePath
testStorePath = "./casper-test-store"

-- | Create a new store, and populate it with a single 'Capser.Var' containing a whole entire
-- 'TrivialTestType'.
trivialTestTypeNewStore
  :: TrivialTestType s
  -> CasperT s IO (Casper.Var (TrivialTestType s) s, Casper.Store s)
trivialTestTypeNewStore root = do
  root <- Casper.transact $ Casper.newVar root
  store <- Casper.getStore
  pure (root, store)

simpleTests :: Spec
simpleTests = do
  let hello = "Hello, world!" :: TestString
  it ("opens a new content addressable store, stores a single value " <> show hello) $
    example $ do
      (root, store) <- Casper.openStore testStorePath initTrivial trivialTestTypeNewStore
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
