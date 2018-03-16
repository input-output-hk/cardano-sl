module Main where

import           Universum

import           Cardano.Wallet.Client (WalletClient (..))
import           System.IO (hSetEncoding, stdout, utf8)

import           CLI
import           Functions
import           Types
import           Error

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

    let walletState = WalletState mempty mempty mempty mempty 0

    -- We throw exception if the value is invalid.
    actionDistr <- either throwM pure actionDistribution

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistr

    pure ()
  where
    actionDistribution :: Either WalletTestError ActionProbabilities
    actionDistribution = do
        postWalletProb <- createProbability 50
        getWalletProb  <- createProbability 50

        pure $  [ (PostWallet, postWalletProb)
                , (GetWallet,  getWalletProb)
                ]

