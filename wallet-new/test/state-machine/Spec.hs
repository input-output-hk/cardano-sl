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

tests :: Handle -> Spec
tests h =
    describe "Tests" $ do
        -- should be the same thing as
        -- ```
        -- before (liftIO $ F.openFile "bla" F.WriteMode) $ after (liftIO . F.hClose)
        -- ```
        -- but for some reason file handle gets closed before shrinking within property
        -- has finished
        --
        -- around (bracket (liftIO $ F.openFile "bla" F.WriteMode) (liftIO . F.hClose)) $
        --
        -- TODO: try to figure out why `around` and/or `after` close handle beforehand
        -- To do so, use simple property which writes something to the file handle, and
        -- returns Fail. This way shrinking will be triggered and we are expecting to see
        -- same results - that handle will get closed and that shrinking won't be able to
        -- write to file handle
        describe "IO" $ do
            -- it "expected failure" $ withMaxSuccess 100 . prop_test_ok
            it "file handle failure" $ once $ prop_test_fail h

testsTasty :: Handle -> TestTree
testsTasty h = testGroup "Tests tasty"
    [ testGroup "IO"
        [ testProperty "file handle failure" $ once $ prop_test_fail h
        ]
    ]

------------------------------------------------------------------------

main :: IO ()
main = do
    bracket
        (liftIO $ F.openFile "bla" F.WriteMode)
        (liftIO . F.hClose)
--        (defaultMain . testsTasty)
        (hspec . tests)
