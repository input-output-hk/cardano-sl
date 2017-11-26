
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (at)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Network.EngineIO (SocketId)

import           Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.QuickCheck (Arbitrary (..), Property, forAll)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)


import           Pos.Crypto (SecretKey)
import           Pos.Explorer.Socket.Methods (addrSubParam, addressSetByTxs, blockPageSubParam,
                                              fromCAddressOrThrow, subscribeAddr, spSessId, txsSubParam)
import           Pos.Explorer.ExplorerMode (runSubTestMode)
import           Pos.Explorer.Socket.Holder (csAddressSubscribers, mkConnectionsState)
import           Pos.Explorer.Web.ClientTypes (CAddress (..), toCAddress)

import           Test.Pos.Explorer.MockFactory (mkTxOut, secretKeyToAddress)

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
        describe "subscribeAddr" $
            modifyMaxSize (const 1) $
                prop "subscribes by a given address" subscribeAddrProp

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

subscribeAddrProp :: Property
subscribeAddrProp =
    forAll arbitrary $ \addr ->
        monadicIO $ do
            -- create an empty ConnectionsState
            let connState = mkConnectionsState
            let cAddr = toCAddress addr
            -- The result of this is `SubscriptionMode m => m ()`
            let subscription = runSubTestMode connState $
                                  subscribeAddr cAddr socketId

            (_, updatedConnState) <- run subscription

            -- get stored sessions by a given `Address`
            let mSessions = updatedConnState ^. csAddressSubscribers . at addr
            -- to check whether a session has been added to it or not
            assert $ hasSession socketId mSessions
  where
    hasSession :: SocketId -> Maybe (S.Set SocketId) -> Bool
    hasSession socketId' (Just sessions) = S.member socketId' sessions
    hasSession _          Nothing        = False
    -- | Create arbitrary, non-null.
    socketId = "testingsocket"

-- | TODO(ks): Maybe this exist already?
instance Arbitrary CAddress where
    arbitrary = toCAddress . secretKeyToAddress <$> arbitrary
