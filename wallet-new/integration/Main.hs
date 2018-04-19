{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http
import qualified Data.ByteString.Char8 as B8
import           Data.Map (fromList)
import           Data.Traversable (for)
import           Data.X509.File (readSignedObject)
import           System.IO (hSetEncoding, stdout, utf8)
import           Test.Hspec

import           AddressSpecs (addressSpecs)
import           CLI
import           Functions
import           TransactionSpecs (transactionSpecs)
import           Types
import           Util (WalletRef, newWalletRef)
import           WalletSpecs (walletSpecs)
import qualified QuickCheckSpecs as QuickCheck

-- | Here we want to run main when the (local) nodes
-- have started.
main :: IO ()
main = do

    hSetEncoding stdout utf8
    CLOptions {..} <- getOptions

    -- stateless

    -- TODO (akegalj): run server cluster in haskell, instead of using shell scripts
    -- serverThread <- async (runWalletServer options)

    printT "Starting the integration testing for wallet."


    when stateless $ do
        printT "The wallet test node is running in stateless mode."
        printT "Stateless mode not implemented currently!"

    let serverId = (serverHost, B8.pack $ show serverPort)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- orFail =<< credentialLoadX509 tlsClientCertPath tlsPrivKeyPath
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let baseUrl = BaseUrl Https serverHost serverPort mempty

    let walletClient :: MonadIO m => WalletClient m
        walletClient = liftClient $ mkHttpClient baseUrl manager

    walletState <- initialWalletState walletClient

    printT $ "Initial wallet state: " <> show walletState

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistribution

    -- Acquire the initial state for the deterministic tests
    wRef <- newWalletRef

    hspec $ deterministicTests wRef walletClient
  where
    orFail :: MonadFail m => Either String a -> m a
    orFail =
        either (fail . ("Error decoding X509 certificates: " <>)) return

    actionDistribution :: ActionProbabilities
    actionDistribution = do
        (PostWallet, Weight 2)
            :| (PostTransaction, Weight 5)
            : fmap (\x -> (x, Weight 1)) [minBound .. maxBound]

initialWalletState :: WalletClient IO -> IO WalletState
initialWalletState wc = do
    -- We will have single genesis wallet in intial state that was imported from launching script
    _wallets <- fromResp $ getWallets wc
    _accounts <- concat <$> for _wallets (fromResp . getAccounts wc . walId)
    -- Lets set all wallet passwords for initial wallets (genesis) to default (emptyPassphrase)
    let _lastAction       = NoOp
        _walletsPass      = fromList $ map ((, V1 mempty) . walId) _wallets
        _addresses        = concatMap accAddresses _accounts
        -- TODO(akegalj): I am not sure does importing a genesis wallet (which we do prior launching integration tests) creates a transaction
        -- If it does, we should add this transaction to the list
        _transactions     = mempty
        _actionsNum       = 0
        _successActions   = mempty
    pure $ WalletState {..}
  where
    fromResp = (either throwM (pure . wrData) =<<)

deterministicTests :: WalletRef -> WalletClient IO -> Spec
deterministicTests wref wc = do
    addressSpecs wref wc
    walletSpecs wref wc
    transactionSpecs wref wc
    QuickCheck.spec
