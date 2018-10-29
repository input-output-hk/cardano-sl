{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           Test.Hspec (Spec, after, around, before, describe, hspec, it,
                     parallel)
import           Test.QuickCheck (ioProperty, once)
import           Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import           Test.Tasty.QuickCheck (testProperty)

import           Wallet

import qualified System.IO as F

------------------------------------------------------------------------

tests :: Spec
tests =
    describe "Tests" $ do
        -- should be the same thing as
        -- ```
        -- before (liftIO $ F.openFile "bla" F.WriteMode) $ after (liftIO . F.hClose)
        -- ```
        around (bracket (liftIO $ F.openFile "bla" F.WriteMode) (liftIO . F.hClose)) $
            describe "IO" $ do
                -- it "expected failure" $ withMaxSuccess 100 . prop_test_ok
                it "file handle failure" $ once . prop_test_fail

testsTasty :: TestTree
testsTasty = testGroup "Tests tasty"
    [ testGroup "IO"
        [ fileHandle "file handle failure" $ once . prop_test_fail
        ]
    ]
  where
    fileHandle test prop =
      withResource (liftIO $ F.openFile "bla" F.WriteMode) (liftIO . F.hClose)
        (\h -> testProperty test (idempotentIOProperty (prop <$> h)))

------------------------------------------------------------------------

main :: IO ()
main = do
--    hspec tests
    defaultMain testsTasty
