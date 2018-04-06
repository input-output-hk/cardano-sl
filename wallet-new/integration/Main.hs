{-# LANGUAGE RankNTypes #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens hiding ((^..), (^?))
import           System.IO (hSetEncoding, stdout, utf8)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           CLI
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

    -- TODO (akegalj): run server cluster in haskell, instead of using shell scripts
    -- serverThread <- async (runWalletServer options)

    printT "Starting the integration testing for wallet."

    when stateless $ do
        printT "The wallet test node is running in stateless mode."
        printT "Stateless mode not implemented currently!"

    -- TODO (akegalj): move these to CLOptions
    let baseUrl = BaseUrl Http "localhost" 8090 mempty
    manager <- newManager defaultManagerSettings

    let walletClient :: MonadIO m => WalletClient m
        walletClient = liftClient $ mkHttpClient baseUrl manager

    walletState <- initialWalletState walletClient

    -- some expected test cases
    hspec $ deterministicTests walletClient

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistribution

    pure ()
  where
    actionDistribution :: ActionProbabilities
    actionDistribution = do
        (PostWallet, Weight 1) :| fmap (\x -> (x, Weight 1)) [minBound .. maxBound]

initialWalletState :: WalletClient IO -> IO  WalletState
initialWalletState wc = do
    _wallets <- either throwM (pure . wrData) =<< getWallets wc
    let _walletsPass = mempty
        _accounts = mempty
        _addresses = mempty
        _transactions = mempty
        _actionsNum = 0
    pure $ WalletState {..}

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
