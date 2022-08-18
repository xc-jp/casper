{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

import Casper (CasperT)
import qualified Casper
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Directory (createDirectory)
import System.IO.Temp (withTempDirectory)
import Test.Hspec (Spec, example, it)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

type TestString = Casper.RawData

newtype TrivialTestType = TrivialTestType [Casper.Ref TestString]
  deriving stock (Generic)
  deriving (Casper.Content)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Serialize) via Casper.WrapAeson TrivialTestType

initTrivial :: TrivialTestType
initTrivial = TrivialTestType []

fixedTestStorePath :: Maybe FilePath
fixedTestStorePath = Nothing

tempDir :: (FilePath -> IO a) -> IO a
tempDir f = case fixedTestStorePath of
  Nothing -> withTempDirectory "." "casper-test-store.XXXXXXXX" f
  Just path -> createDirectory path >> f path

simpleTests :: Spec
simpleTests =
  let hello = "Hello, world!" :: TestString
   in it ("opens a new content addressable store, stores a single value " <> show hello) $
        example $
          tempDir $ \testStorePath ->
            Casper.openStore testStorePath initTrivial $ \rootVar -> do
              Casper.transact $ do
                ref <- Casper.newRef hello
                (TrivialTestType refList) <- Casper.readVar rootVar
                Casper.writeVar rootVar $ TrivialTestType $ ref : refList
              Casper.transact $ do
                (TrivialTestType refList) <- Casper.readVar rootVar
                values <- traverse Casper.readRef refList
                unless (values == [hello]) $
                  error $ "expecting ref to contain: " <> show [hello] <> ", got " <> show values

main :: IO ()
main = do
  specs <- testSpecs simpleTests
  let tests = testGroup "Casper" [testGroup "Simple Tests" specs]
  defaultMain tests
