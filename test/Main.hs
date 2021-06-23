{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Casper
import Control.Exception
import Internal (CasperError (..), Store (..), blobPath, hashBS)
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Control.Concurrent.STM.TVar(newTVarIO)
import qualified Data.Set as Set

spec :: Spec
spec = describe "SHA256" $ do
  it "should give path to content" $ do
    lock <- newTVarIO False
    activeBlocks <- newTVarIO 0
    blobPath (hashBS "bar") (Store "foo" lock activeBlocks) `shouldBe` "foo/blob/_N4rLtula_QIYB-3If6bXDONEO5CnqBPrlURto-_j7k="

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

inTemporaryStore :: (Casper.Store -> IO b) -> IO b
inTemporaryStore f = withSystemTempDirectory "temp" $ \tmp -> initStore (tmp </> "store") >>= f

-- | Storing and then retrieving the stored item should always return the stored contents
-- >>> store a >>= retrieve = a
prop_store_retreive :: Int -> Property
prop_store_retreive a = monadicIO $ run $ inTemporaryStore $ \root -> do
  result <- runCasperT root $ store a >>= retrieve
  pure $ result === Right a

-- | This test transitively tests that:
-- >>> store a >>= retrieve = store a >> store a >>= retrieve
-- given 'prop_store_retrieve'
prop_store_store_retreive :: Int -> Property
prop_store_store_retreive a = monadicIO $ run $ inTemporaryStore $ \root -> do
  result <- runCasperT root $ store a >> store a >>= retrieve
  pure $ result === Right a

-- | Retrieving an address after collecting garbage with no roots should yield an error
-- >>> store a >> collectGarbage >> retrieve = Left _
prop_retrive_after_colllect_garbage :: Int -> Property
prop_retrive_after_colllect_garbage a = monadicIO $ run $ inTemporaryStore $ \root -> do
  Right addr <- runCasperT root $ store a
  collectGarbage root (pure mempty)
  result <- runCasperT root $ retrieve addr
  pure $ result === Left (FileMissing (blobPath sha root))
  where
    sha = hashBS (encodeContent a)

-- | If the stored address is marked as a root it should not get removed
-- store a >> markRoot a >> collectGarbage >> retrieve = a
prop_retrive_after_colllect_garbage_with_root :: Int -> Property
prop_retrive_after_colllect_garbage_with_root a = monadicIO $ run $ inTemporaryStore $ \root -> do
  Right addr <- runCasperT root $ store a
  collectGarbage root (pure (Set.singleton (forget addr)))
  result <- runCasperT root $ retrieve addr
  pure $ result === Right a

-- How do generate an arbitrary CasperT m a action?
-- runCasperT store act1 >> runCasperT store act2 = runCasperT store (act1 >> act2)

-- Test something with references

-- Inject quickcheck properties here
return []

quickTest :: TestTree
quickTest = testProperties "quickcheck" $allProperties

main :: IO ()
main = do
  specTest <- testSpec "spec" spec
  defaultMain
    ( testGroup
        "tests"
        [ specTest,
          quickTest
        ]
    )
