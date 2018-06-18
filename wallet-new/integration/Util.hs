{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import           Universum hiding ((^?))

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck (Gen, arbitrary, generate)

import           Pos.Crypto.Signing (PublicKey, encToPublic, encodeBase58PublicKey)
import           Pos.Util.BackupPhrase (safeKeysFromPhrase)


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

randomExternalWallet :: WalletOperation -> IO NewExternalWallet
randomExternalWallet walletOp =
    generate $
        NewExternalWallet
            <$> (encodeBase58PublicKey <$> arbitraryExtPubKey)
            <*> arbitrary
            <*> pure "External Wallet"
            <*> pure walletOp
  where
    arbitraryExtPubKey :: Gen PublicKey
    arbitraryExtPubKey =
        (encToPublic . fst . orFail . safeKeysFromPhrase mempty) <$> arbitrary

    orFail :: (Show e) => Either e a -> a
    orFail =
        either (error . show) identity

createWalletCheck :: WalletClient IO -> NewWallet -> IO Wallet
createWalletCheck wc newWallet = do
    result <- fmap wrData <$> postWallet wc newWallet
    result `shouldPrism` _Right

createExternalWalletCheck :: WalletClient IO -> NewExternalWallet -> IO Wallet
createExternalWalletCheck wc newExtWallet = do
    result <- fmap wrData <$> postExternalWallet wc newExtWallet
    result `shouldPrism` _Right

firstAccountAndId :: WalletClient IO -> Wallet -> IO (Account, WalletAddress)
firstAccountAndId wc wallet = do
    toAccts <- accountsInWallet wc wallet
    let (toAcct : _) = toAccts
        (toAddr : _) = accAddresses toAcct
    pure (toAcct, toAddr)

firstAccountInExtWallet :: WalletClient IO -> Wallet -> IO Account
firstAccountInExtWallet wc wallet = do
    toAccts <- accountsInWallet wc wallet
    let (fstAcct : _) = toAccts
    pure fstAcct

accountsInWallet :: WalletClient IO -> Wallet -> IO [Account]
accountsInWallet wc wallet = do
    etoAccts <- getAccounts wc (walId wallet)
    toAccts <- fmap wrData etoAccts `shouldPrism` _Right
    toAccts `shouldSatisfy` (not . null)
    pure toAccts

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
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find ((lockedWallet /=) . walId) allWallets)

-- Hard code the genesis wallet that's asset locked
genesisAssetLockedWallet :: WalletClient IO -> IO Wallet
genesisAssetLockedWallet wc = do
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find ((lockedWallet ==) . walId) allWallets)

lockedWallet :: WalletId
lockedWallet =
    WalletId "Ae2tdPwUPEZ5YjF9WuDoWfCZLPQ56MdQC6CZa2VKwMVRVqBBfTLPNcPvET4"

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

shouldFailWith
    :: (Show a)
    => Either ClientError (WalletResponse a)
    -> ClientError
    -> IO ()
shouldFailWith eresp wantErr = do
    gotErr <- eresp `shouldPrism` _Left
    gotErr `shouldBe` wantErr
