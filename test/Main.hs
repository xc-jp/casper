{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Casper
import Casper.Internal (CasperError (..), Store (..), blobPath, hashBS, resourceClosure)
import Casper.Resource (Loc (Loc), Resource (encodeResource))
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (fromString)
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, expectationFailure, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (testProperties)

spec :: Spec
spec = do
  describe "SHA256" $ do
    it "should give path to content" $
      withSystemTempDirectory "temp" $ \tmp -> do
        s <- initStore tmp tmp
        blobPath (hashBS "bar") s `shouldBe` (tmp </> "blob" </> "_N4rLtula_QIYB-3If6bXDONEO5CnqBPrlURto-_j7k=")
  describe "resources" $ do
    describe "nested" $ do
      it "should store" $
        inTemporaryStore [] $ \s -> do
          result <- runCasperT s $ do
            root <- getRoot
            r0 <- newResource (1337 :: Int)
            writeResource root [r0]
          result `shouldBe` Right ()
      it "should maintain resources" $
        inTemporaryStore [] $ \s -> do
          runCasperT s $ do
            root <- getRoot
            r0 <- newResource (1337 :: Int)
            r1 <- newResource (42 :: Int)
            writeResource root [r1, r0]
          collectGarbage s
          result <- runCasperT s $ do
            root <- getRoot
            readResource root >>= traverse readResource
          result `shouldBe` Right [42, 1337]
      it "should allow resource Locs as keys in resource" $
        inTemporaryStore mempty $ \s -> do
          result <- runCasperT s $ do
            root <- getRoot
            r0 <- newResource (1234 :: Int)
            r1 <- newResource (1337 :: Int)
            writeResource root (Map.fromList [(r0, "hello" :: String), (r1, "world")])
            x <- readResource root
            pure (r0, r1, encodeResource x)
          case result of
            Right (Loc uuid, Loc uuid', encoded) ->
              let quoted :: Show a => a -> String
                  quoted = show . show
                  hello =
                    [ "[",
                      quoted uuid,
                      ",",
                      show "hello",
                      "]"
                    ]
                  world =
                    [ "[",
                      quoted uuid',
                      ",",
                      show "world",
                      "]"
                    ]
                  sorted =
                    if uuid < uuid'
                      then hello <> [","] <> world
                      else world <> [","] <> hello
                  expected = mconcat $ ["["] <> sorted <> ["]"]
               in encoded `shouldBe` fromString expected
            Left err -> expectationFailure $ show err
      it "can reference content" $
        inTemporaryStore (Nothing, Nothing) $ \s -> do
          result <- runCasperT s $ do
            root <- getRoot
            r0 <- newResource (1337 :: Int)
            ref <- store (42 :: Int)
            writeResource root (Just r0, Just ref)
            (a, b) <- readResource root
            a' <- traverse readResource a
            b' <- traverse retrieve b
            pure (a', b')
          result `shouldBe` Right (Just 1337, Just 42)
  describe "content" $
    it "cannot reference resources" $ do
      inTemporaryStore (Nothing :: Maybe (Ref Int)) $ \s -> do
        result <- runCasperT s $ do
          root <- getRoot
          -- type error: Ref Int is not Content
          -- ref <- store root
          ref <- store (42 :: Int)
          writeResource root (Just ref)
          retrieve ref
        result `shouldBe` Right 42

inTemporaryStore :: Resource root => root -> (Casper.Store root -> IO b) -> IO b
inTemporaryStore root f = withSystemTempDirectory "temp" $ \tmp -> initStore (tmp </> "store") root >>= f

-- | Storing and then retrieving the stored item should always return the stored contents
-- >>> store a >>= retrieve = a
prop_store_retreive :: Int -> Property
prop_store_retreive a = monadicIO $
  run $
    inTemporaryStore () $ \dir -> do
      result <- runCasperT dir $ store a >>= retrieve
      pure $ result === Right a

-- | This test transitively tests that:
-- >>> store a >>= retrieve = store a >> store a >>= retrieve
-- given 'prop_store_retrieve'
prop_store_store_retreive :: Int -> Property
prop_store_store_retreive a = monadicIO $
  run $
    inTemporaryStore () $ \root -> do
      result <- runCasperT root $ store a >> store a >>= retrieve
      pure $ result === Right a

-- | Retrieving an address after collecting garbage that's not accessible from
-- the root resource should yield a @FileMissing@ error.
-- -- >>> store a >> collectGarbage >> retrieve = Left _
prop_retrive_after_colllect_garbage :: Int -> Property
prop_retrive_after_colllect_garbage a = monadicIO $
  run $
    inTemporaryStore () $ \root -> do
      Right addr <- runCasperT root $ store a
      collectGarbage root
      result <- runCasperT root $ retrieve addr
      pure $ result === Left (FileMissing (blobPath sha root))
  where
    sha = hashBS (encodeContent a)

-- | If the stored address is accessible from the root resource
-- we should not garbage collect the content.
-- store a >> markRoot a >> collectGarbage >> retrieve = a
prop_retrive_after_colllect_garbage_with_root :: Int -> Property
prop_retrive_after_colllect_garbage_with_root a = monadicIO $
  run $
    inTemporaryStore Nothing $ \root -> do
      Right addr <- runCasperT root $ do
        ref <- store a
        topLevel <- getRoot
        writeResource topLevel (Just ref)
        pure ref
      collectGarbage root
      result <- runCasperT root $ retrieve addr
      pure $ result === Right a

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
