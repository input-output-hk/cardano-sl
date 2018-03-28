{-# LANGUAGE RankNTypes #-}

module Main where

import           Universum

import           Cardano.Wallet.Client
import           Control.Lens hiding ((^..), (^?))
import           System.IO (hSetEncoding, stdout, utf8)
import           Test.QuickCheck (arbitrary, generate)
import           Test.Hspec

import           CLI
import           Error
import           Functions
import           Types

-- | Here we want to run main when the (local) nodes
-- have started.
main :: IO ()
main = do

    hSetEncoding stdout utf8
    CLOptions {..} <- getOptions

    _pubCert <- readFile tlsPubCertPath
    _privKey <- readFile tlsPrivKeyPath
    -- stateless

    printT "Starting the integration testing for wallet."

    when stateless $ do
        printT "The wallet test node is running in stateless mode."
        printT "Stateless mode not implemented currently!"

    let walletClient :: forall m. WalletClient m
        walletClient = error "Missing implementation for client!"

    let walletState = WalletState mempty mempty mempty mempty mempty 0

    -- We throw exception if the value is invalid.
    actionDistr <- either throwM pure actionDistribution

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistr

    -- some expected test cases
    hspec $ deterministicTests walletClient

    pure ()
  where
    actionDistribution :: Either WalletTestError ActionProbabilities
    actionDistribution = do
        postWalletProb <- createProbability 50
        getWalletProb  <- createProbability 50

        pure $  [ (PostWallet, postWalletProb)
                , (GetWallet,  getWalletProb)
                ]

deterministicTests :: WalletClient IO -> Spec
deterministicTests wc = do
    describe "Addresses" $ do
        it "Creating an address makes it available" $ do
            -- create a wallet
            newWallet <- generate $
                NewWallet
                    <$> arbitrary
                    <*> pure Nothing
                    <*> arbitrary
                    <*> pure "Wallet"
                    <*> pure CreateWallet
            result <- fmap wrData <$> postWallet wc newWallet
            Wallet{..} <- result `shouldPrism` _Right

            -- create an account
            accResp <- postAccount wc walId (NewAccount Nothing "hello")
            Account{..} <- wrData <$> accResp `shouldPrism` _Right

            -- create an address
            addResp <- postAddress wc (NewAddress Nothing accIndex walId)
            addr <- wrData <$> addResp `shouldPrism` _Right

            -- verify that address is in the API
            idxResp <- getAddressIndex wc
            addrs <- wrData <$> idxResp `shouldPrism` _Right

            addr `shouldSatisfy` (`elem` addrs)

        it "Index returns real data" $ do
            addrsResp <- getAddressIndex wc
            addrs <- wrData <$> addrsResp `shouldPrism` _Right

            addrsResp' <- getAddressIndex wc
            addrs' <- wrData <$> addrsResp' `shouldPrism` _Right

            addrs `shouldBe` addrs'

shouldPrism :: Show s => s -> Prism' s a -> IO a
shouldPrism a b = do
    a `shouldSatisfy` has b
    let Just x = a ^? b
    pure x

infixr 8 `shouldPrism`
