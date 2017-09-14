
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Data.ByteString.Char8             (pack)
import           Test.Hspec                        (Spec, describe, it, shouldBe, shouldThrow, anyException)

import           Pos.Explorer.Socket.Methods       (addrSubParam, blockPageSubParam, fromCAddressOrThrow, spSessId, txsSubParam)
import           Pos.Explorer.Web.ClientTypes      (CAddress(..))

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "Methods" $ do
        describe "fromCAddressOrThrow" $
            it "throws an exception if a given CAddress is invalid" $
                fromCAddressOrThrow (CAddress "invalid" ) `shouldThrow` anyException
        describe "addrSubParam" $
            it "stores a given SocketId into SubscriptionParam of address subscribers" $ do
                let socketId = pack "any-id" -- SocketId
                    subParam = addrSubParam socketId
                spSessId subParam `shouldBe` socketId
        describe "blockPageSubParam" $
            it "stores a given SocketId into SubscriptionParam of blocks subscribers" $ do
                let socketId = pack "any-id" -- SocketId
                    subParam = blockPageSubParam socketId
                spSessId subParam `shouldBe` socketId
        describe "txsSubParam" $
            it "stores a given SocketId into SubscriptionParam of txs subscribers" $ do
                let socketId = pack "any-id" -- SocketId
                    subParam = txsSubParam socketId
                spSessId subParam `shouldBe` socketId
