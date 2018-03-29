module Main where

import           Universum

import           Cardano.Wallet.Client (WalletClient (..), liftClient)
import           Cardano.Wallet.Client.Http (mkHttpClient)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.Client (BaseUrl (..), Scheme (Http))
import           System.IO (hSetEncoding, stdout, utf8)

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

    -- TODO (akegalj): run server cluster in haskell, instead of using shell scripts
    -- serverThread <- async (runWalletServer options)

    printT "Starting the integration testing for wallet."

    when stateless $ do
        printT "The wallet test node is running in stateless mode."
        printT "Stateless mode not implemented currently!"

    -- TODO (akegalj): move these to CLOptions
    let baseUrl = BaseUrl Http "127.0.0.1" 8090 mempty
    manager <- newManager defaultManagerSettings

    let walletClient :: MonadIO m => WalletClient m
        walletClient = liftClient $ mkHttpClient baseUrl manager

    let walletState = WalletState mempty mempty mempty mempty mempty 0

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

        pure $ (PostWallet, postWalletProb) :|
             [ (GetWallet, getWalletProb)
             ]

