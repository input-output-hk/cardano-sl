{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import           Universum hiding ((^?))

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)


type WalletRef = MVar Wallet

randomWallet :: WalletOperation -> IO NewWallet
randomWallet walletOp =
    generate $
        NewWallet
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
            <*> pure "Wallet"
            <*> pure walletOp

randomCreateWallet :: IO NewWallet
randomCreateWallet = randomWallet CreateWallet

randomRestoreWallet :: IO NewWallet
randomRestoreWallet = randomWallet RestoreWallet

createWalletCheck :: WalletClient IO -> NewWallet -> IO Wallet
createWalletCheck wc newWallet = do
    result <- fmap wrData <$> postWallet wc newWallet
    result `shouldPrism` _Right

firstAccountAndId :: WalletClient IO -> Wallet -> IO (Account, WalletAddress)
firstAccountAndId wc wallet = do
    etoAccts <- getAccounts wc (walId wallet)
    toAccts <- fmap wrData etoAccts `shouldPrism` _Right

    toAccts `shouldSatisfy` (not . null)
    let (toAcct : _) = toAccts

    accAddresses toAcct `shouldSatisfy` (not . null)
    let (toAddr : _) = accAddresses toAcct

    pure (toAcct, toAddr)

newWalletRef :: IO WalletRef
newWalletRef = newEmptyMVar

sampleWallet :: WalletRef -> WalletClient IO -> IO Wallet
sampleWallet wRef wc = do
    mwallet <- tryTakeMVar wRef
    case mwallet of
        Just wallet -> do
            putMVar wRef wallet
            pure wallet
        Nothing -> do
            w <- randomWallet CreateWallet
            w' <- createWalletCheck wc w
            didWrite <- tryPutMVar wRef w'
            if didWrite
                then pure w'
                else readMVar wRef

genesisWallet :: WalletClient IO -> IO Wallet
genesisWallet wc = do
    mwallet <- tryTakeMVar genesisRef
    case mwallet of
        Just wallet -> do
            putMVar genesisRef wallet
            pure wallet
        Nothing -> do
            Right allWallets <- fmap wrData <$> getWallets wc
            wallet <- maybe
                (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
                return
                (find (("Genesis wallet" ==) . walName) allWallets)
            didWrite <- tryPutMVar genesisRef wallet
            if didWrite
                then pure wallet
                else readMVar genesisRef

genesisRef :: WalletRef
genesisRef = unsafePerformIO newEmptyMVar
{-# NOINLINE genesisRef #-}

shouldPrism :: Show s => s -> Prism' s a -> IO a
shouldPrism a b = do
    a `shouldSatisfy` has b
    let Just x = a ^? b
    pure x

infixr 8 `shouldPrism`

shouldPrism_ :: Show s => s -> Prism' s a -> IO ()
shouldPrism_ a b =
    a `shouldSatisfy` has b

infixr 8 `shouldPrism_`
