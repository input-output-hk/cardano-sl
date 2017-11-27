
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
import           Test.QuickCheck (Property, arbitrary, forAll)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Crypto (SecretKey)
import           Pos.Explorer.ExplorerMode (runSubTestMode)
import           Pos.Explorer.Socket.Holder (ConnectionsState, ExplorerSocket(..),
                                             csAddressSubscribers,
                                             csBlocksPageSubscribers, csClients,
                                             mkClientContext, mkConnectionsState)
import           Pos.Explorer.Socket.Methods (addrSubParam, addressSetByTxs,
                                              blockPageSubParam, fromCAddressOrThrow,
                                              spSessId, subscribeAddr, subscribeBlocksLastPage, txsSubParam)
import           Pos.Explorer.TestUtil (secretKeyToAddress)
import           Pos.Explorer.Web.ClientTypes (CAddress (..), toCAddress)

import           Test.Pos.Explorer.MockFactory (mkTxOut)


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
            modifyMaxSize (const 200) $
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
            modifyMaxSize (const 200) $
                prop "adds sessions of subscribers by a given `Address` to ConnectionsState" subscribeAddrProp
        describe "subscribeBlocksLastPage" $
            modifyMaxSize (const 200) $
                prop "adds sessions of subscribers to ConnectionsState" subscribeBlocksLastPageProp


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
    forAll arbitrary $ \(socketId, addr) ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            let cAddr = toCAddress addr
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



subscribeBlocksLastPageProp :: Property
subscribeBlocksLastPageProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            let subscription = runSubTestMode connState $
                                    subscribeBlocksLastPage socketId

            (_, updatedConnState) <- run subscription

            -- get sessions of "block last page" subscribers
            let mSessions = updatedConnState ^. csBlocksPageSubscribers
            -- to check whether a session has been added to it or not
            assert $ S.member socketId mSessions

-- | Helper to create a "subscription-able" `ConnectionsState`
mkSubConnectionState :: SocketId -> ConnectionsState
mkSubConnectionState socketId =
    let ctx = mkClientContext $ TestSocket "explorer-test-socket" in
    mkConnectionsState & csClients . at socketId .~ Just ctx
