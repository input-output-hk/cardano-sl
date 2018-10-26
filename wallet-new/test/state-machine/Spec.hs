{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           Test.Hspec (Spec, after, around, before, describe, hspec, it,
                     parallel)
import           Test.QuickCheck (expectFailure, once, withMaxSuccess)

import           Wallet

import qualified System.IO as F

------------------------------------------------------------------------

tests :: Spec
tests =
    describe "Tests" $ parallel $ do
        before (liftIO $ F.openFile "bla" F.WriteMode) $ after (liftIO . F.hClose) $
            describe "IO" $ do
                it "expected failure" $ withMaxSuccess 100 . prop_test_ok
                it "file handle failure" $ withMaxSuccess 100 . prop_test_fail

------------------------------------------------------------------------

main :: IO ()
main = do
    hspec tests
