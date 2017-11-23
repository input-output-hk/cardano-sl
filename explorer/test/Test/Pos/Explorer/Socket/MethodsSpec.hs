
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.State.Class (MonadState (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Network.EngineIO (SocketId)

import           Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.Pos.Block.Logic.Emulation (Emulation(..))
import           Test.Pos.Util (withDefConfigurations)
import           Test.QuickCheck (Arbitrary (..), Property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Crypto (SecretKey)
import           Pos.Explorer.Socket.Methods (SubscriptionMode, addrSubParam, addressSetByTxs, blockPageSubParam,
                                              fromCAddressOrThrow, subscribeAddr, spSessId, txsSubParam)
import           Pos.Explorer.ExplorerMode (SubscriptionTestParams, runSubscriptionTestMode)
import           Pos.Explorer.Socket.Holder (ConnectionsState(..), ccAddress, csClients, mkConnectionsState)
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

subscribeAddrAssert
  :: (SubscriptionMode m, MonadIO m)
  => CAddress -> SocketId -> m Bool
subscribeAddrAssert cAddr socketId = do
    -- create an emtpy ConnectionsState
    connState <- atomically $ newTVar mkConnectionsState
    -- subscribe to an `CAddress`
    subscribeAddr cAddr socketId
    -- get an updated ConnectionsState ...
    connState' <- atomically $ readTVar connState
    -- to check if CAddress` has been added to it
    let ctx = connState' ^. (csClients . at socketId)
    pure $ maybe False hasAddress ctx
  where
    hasAddress ctx' = maybe False ((==) cAddr . toCAddress) $ ctx' ^. ccAddress

subscribeAddrProp
    :: SubscriptionTestParams
    -> CAddress
    -> SocketId
    -> Property
subscribeAddrProp testParams addr socketId = monadicIO $ do
    let subscription = withDefConfigurations $
                            runSubscriptionTestMode testParams $
                            subscribeAddrAssert addr socketId
    result <- run subscription
    assert result

-- TODO (jk): Move all following instances to other, more common modules
-- eg. to TestUtil or similar

instance MonadState ConnectionsState Emulation where
    get = Emulation get
    put newState = Emulation $ put newState

-- TODO (jk) Fix this instance
instance MonadState ConnectionsState IO where
    get = get
    put = put

instance Arbitrary CAddress where
    arbitrary = toCAddress . secretKeyToAddress <$> arbitrary
