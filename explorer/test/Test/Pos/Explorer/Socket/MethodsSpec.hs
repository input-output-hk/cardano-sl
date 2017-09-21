
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString.Char8             as BS
import qualified Data.Set                          as S
import qualified Data.List.NonEmpty                as NE

import           Test.Hspec                        (Spec, describe, it, shouldBe, shouldThrow, anyException)
import           Test.Hspec.QuickCheck             (modifyMaxSize, prop)

import           Pos.Crypto                        (SecretKey)
import           Pos.Explorer.Socket.Methods       (addressSetByTxs, addrSubParam, blockPageSubParam, fromCAddressOrThrow, spSessId, txsSubParam)
import           Pos.Explorer.Web.ClientTypes      (CAddress(..))
import           Test.Pos.Explorer.MockFactory     (mkTxOut, secretKeyToAddress)


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
        describe "addressSetByTxs" $
            modifyMaxSize (const 50) $
                prop "creates a Set of Addresses by given txs"
                    addressSetByTxsProp
        describe "addrSubParam" $
            it "stores a given SocketId into SubscriptionParam of address subscribers" $ do
                let socketId = BS.pack "any-id" -- SocketId
                    subParam = addrSubParam socketId
                spSessId subParam `shouldBe` socketId
        describe "blockPageSubParam" $
            it "stores a given SocketId into SubscriptionParam of blocks subscribers" $ do
                let socketId = BS.pack "any-id" -- SocketId
                    subParam = blockPageSubParam socketId
                spSessId subParam `shouldBe` socketId
        describe "txsSubParam" $
            it "stores a given SocketId into SubscriptionParam of txs subscribers" $ do
                let socketId = BS.pack "any-id" -- SocketId
                    subParam = txsSubParam socketId
                spSessId subParam `shouldBe` socketId

addressSetByTxsProp :: SecretKey -> Bool
addressSetByTxsProp key =
    let
        addrA = secretKeyToAddress key
        addrB = secretKeyToAddress key
        txA = mkTxOut 2 addrA
        txB = mkTxOut 3 addrA
        txC = mkTxOut 4 addrB
        txsNE = NE.fromList [[txA, txB, txC]]
        txNE = NE.fromList [txA]
        addrs = addressSetByTxs txNE txsNE
    in
        addrs == S.fromList [addrA]
