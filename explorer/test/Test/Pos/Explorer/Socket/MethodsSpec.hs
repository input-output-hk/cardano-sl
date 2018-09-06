-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (at)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Network.EngineIO (SocketId)

import           Test.Hspec (Spec, anyException, describe, it, runIO, shouldBe, shouldThrow)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.QuickCheck (Property, arbitrary, forAll, generate)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..), SecretKey)
import           Pos.Explorer.ExplorerMode (runSubTestMode)
import           Pos.Explorer.Socket.Holder (ConnectionsState, ExplorerSocket (..),
                                             csAddressSubscribers, csBlocksPageSubscribers,
                                             csClients, csEpochsLastPageSubscribers,
                                             csTxsSubscribers, mkClientContext, mkConnectionsState)
import           Pos.Explorer.Socket.Methods (addrSubParam, addressSetByTxs, blockPageSubParam,
                                              fromCAddressOrThrow, spSessId, subscribeAddr,
                                              subscribeBlocksLastPage, subscribeEpochsLastPage,
                                              subscribeTxs, txsSubParam, unsubscribeAddr,
                                              unsubscribeBlocksLastPage, unsubscribeEpochsLastPage,
                                              unsubscribeFully, unsubscribeTxs)
import           Pos.Explorer.TestUtil (secretKeyToAddress)
import           Pos.Explorer.Web.ClientTypes (CAddress (..), toCAddress)

import           Test.Pos.Explorer.MockFactory (mkTxOut)


----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody (makeNetworkMagic pm)

specBody :: NetworkMagic -> Spec
specBody nm =
    describe "Methods" $ do
        describe "fromCAddressOrThrow" $
            it "throws an exception if a given CAddress is invalid" $
                fromCAddressOrThrow (CAddress "invalid" ) `shouldThrow` anyException
        describe "addressSetByTxs" $
            modifyMaxSize (const 200) $
                prop "creates a Set of Addresses by given txs"
                    (addressSetByTxsProp nm)
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
                prop "adds sessions of `Address` subscribers to `ConnectionsState`"
                    subscribeAddrProp
        describe "unsubscribeAddr" $
            modifyMaxSize (const 200) $
                prop "removes a session of an `Address` subscriber from `ConnectionsState`"
                    unsubscribeAddrProp
        describe "subscribeBlocksLastPage" $
            modifyMaxSize (const 200) $
                prop "adds sessions of `block last page` subscribers to `ConnectionsState`"
                    subscribeBlocksLastPageProp
        describe "unsubscribeBlocksLastPage" $
            modifyMaxSize (const 200) $
                prop "removes sessions of `block last page` subscribers from `ConnectionsState`"
                    unsubscribeBlocksLastPageProp
        describe "subscribeTxs" $
            modifyMaxSize (const 200) $
                prop "adds sessions of `tx` subscribers to `ConnectionsState`"
                    subscribeTxsProp
        describe "unsubscribeTxs" $
            modifyMaxSize (const 200) $
                prop "removes sessions of `tx` subscribers from `ConnectionsState`"
                    unsubscribeTxsProp
        describe "subscribeEpochsLastPage" $
            modifyMaxSize (const 200) $
                prop "adds sessions of `epochs last page` subscribers to `ConnectionsState`"
                    subscribeEpochsLastPageProp
        describe "unsubscribeEpochsLastPage" $
            modifyMaxSize (const 200) $
                prop "removes sessions of `epochs last page` subscribers from `ConnectionsState`"
                    unsubscribeEpochsLastPageProp
        describe "unsubscribeFully" $
            modifyMaxSize (const 200) $
                prop "removes all sessions of subscribers from `ConnectionsState`"
                    unsubscribeFullyProp


addressSetByTxsProp :: NetworkMagic -> SecretKey -> Bool
addressSetByTxsProp _nm key =
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

            -- get sessions `Address` subscribers
            let mSessions = updatedConnState ^. csAddressSubscribers . at addr
            -- to check whether current session has been added to it or not
            assert $ hasSession socketId mSessions
  where
    hasSession :: SocketId -> Maybe (S.Set SocketId) -> Bool
    hasSession socketId' (Just sessions) = S.member socketId' sessions
    hasSession _          Nothing        = False

unsubscribeAddrProp :: Property
unsubscribeAddrProp =
    forAll arbitrary $ \(socketId, addr) ->
        monadicIO $ do
            let cAddr = toCAddress addr
            let connState = mkSubConnectionState socketId
            -- Add a subscription first ...
            let subscription = runSubTestMode connState $
                                  subscribeAddr cAddr socketId
            (_, updatedConnState) <- run subscription
            -- ... and remove subscription
            let unsubscription = runSubTestMode updatedConnState $
                                    unsubscribeAddr socketId
            (_, updatedConnState') <- run unsubscription

            -- get sessions of `Address` subscribers
            let sessions = updatedConnState' ^. csAddressSubscribers
            -- to check that no session has been stored anymore
            assert $ M.size sessions == 0

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
subscribeBlocksLastPageProp :: Property
subscribeBlocksLastPageProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            let subscription = runSubTestMode connState $
                                    subscribeBlocksLastPage socketId

            (_, updatedConnState) <- run subscription

            -- get sessions of "block last page" subscribers
            let sessions = updatedConnState ^. csBlocksPageSubscribers
            -- to check whether current session has been added to it or not
            assert $ S.member socketId sessions

unsubscribeBlocksLastPageProp :: Property
unsubscribeBlocksLastPageProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            -- add a subscription ...
            let subscription = runSubTestMode connState $
                                    subscribeBlocksLastPage socketId
            (_, updatedConnState) <- run subscription
            -- and unsubscribe it
            let unsubscription = runSubTestMode updatedConnState $
                                    unsubscribeBlocksLastPage socketId
            (_, updatedConnState') <- run unsubscription

            -- get sessions of "block last page" subscribers
            let sessions = updatedConnState' ^. csBlocksPageSubscribers
            -- to check that this session has not been stored anymore
            assert $ S.size sessions == 0


subscribeTxsProp :: Property
subscribeTxsProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            let subscription = runSubTestMode connState $
                                    subscribeTxs socketId

            (_, updatedConnState) <- run subscription

            -- get sessions of "tx" subscribers
            let sessions = updatedConnState ^. csTxsSubscribers
            -- to check whether current session has been added to it or not
            assert $ S.member socketId sessions


unsubscribeTxsProp :: Property
unsubscribeTxsProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            -- add a subscription ...
            let connState = mkSubConnectionState socketId
            let subscription = runSubTestMode connState $
                                    subscribeTxs socketId
            (_, updatedConnState) <- run subscription
            -- .. and unsubscribe it
            let unsubscription = runSubTestMode updatedConnState $
                                    unsubscribeTxs socketId
            (_, updatedConnState') <- run unsubscription
            -- get sessions of "tx" subscribers
            let sessions = updatedConnState' ^. csTxsSubscribers
            -- to check current session has been removed
            assert $ S.size sessions == 0


subscribeEpochsLastPageProp :: Property
subscribeEpochsLastPageProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            let subscription = runSubTestMode connState $
                                    subscribeEpochsLastPage socketId

            (_, updatedConnState) <- run subscription

            -- get sessions of "epoch last page" subscribers
            let sessions = updatedConnState ^. csEpochsLastPageSubscribers
            -- to check whether current session has been added to it or not
            assert $ S.member socketId sessions


unsubscribeEpochsLastPageProp :: Property
unsubscribeEpochsLastPageProp =
    forAll arbitrary $ \socketId ->
        monadicIO $ do
            let connState = mkSubConnectionState socketId
            -- Add a subscription ...
            let subscription = runSubTestMode connState $
                                    subscribeEpochsLastPage socketId
            (_, updatedConnState) <- run subscription
            -- and unsubscribe it
            let unsubscription = runSubTestMode updatedConnState $
                                      unsubscribeEpochsLastPage socketId
            (_, updatedConnState') <- run unsubscription

            -- get sessions of "epoch last page" subscribers
            let sessions = updatedConnState' ^. csEpochsLastPageSubscribers
            -- to check that current session has been removed
            assert $ S.size sessions == 0


unsubscribeFullyProp :: Property
unsubscribeFullyProp =
    forAll arbitrary $ \(socketId, cAddr) ->
        monadicIO $ do
            -- create `Address` subscription
            let connState = mkSubConnectionState socketId
            let subscriptionA = runSubTestMode connState $
                                  subscribeAddr cAddr socketId
            (_, connStateA) <- run subscriptionA
            -- create `Txs` subscription
            let subscriptionB = runSubTestMode connStateA $
                                  subscribeTxs socketId
            (_, connStateB) <- run subscriptionB
            -- create `blocks last page` subscription
            let subscriptionC = runSubTestMode connStateB $
                                  unsubscribeBlocksLastPage socketId
            (_, connStateC) <- run subscriptionC
            -- create `epochs last page` subscription
            let subscriptionD = runSubTestMode connStateC $
                                  subscribeEpochsLastPage socketId
            (_, connStateD) <- run subscriptionD

            -- unsubscribe all
            let unsubscription = runSubTestMode connStateD $
                                    unsubscribeFully socketId
            (_, connStateFinal) <- run unsubscription

            -- get all sessions of subscribers
            let sessionsA = connStateFinal ^. csAddressSubscribers
            let sessionsB = connStateFinal ^. csBlocksPageSubscribers
            let sessionsC = connStateFinal ^. csTxsSubscribers
            let sessionsD = connStateFinal ^. csEpochsLastPageSubscribers
            -- to check that all sessions have to be removed
            assert $ (M.size sessionsA == 0) &&
                     (S.size sessionsB == 0) &&
                     (S.size sessionsC == 0) &&
                     (S.size sessionsD == 0)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Creates a "subscription-able" `ConnectionsState`
mkSubConnectionState :: SocketId -> ConnectionsState
mkSubConnectionState socketId =
    let ctx = mkClientContext $ TestSocket "explorer-test-socket" in
    mkConnectionsState & csClients . at socketId .~ Just ctx
