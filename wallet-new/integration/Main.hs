{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http
import           Data.Map (fromList)
import           Data.Traversable (for)
import           Data.X509.File (readSignedObject)
import           Network.HTTP.Client (Manager)
import           System.Environment (withArgs)
import           System.IO (hSetEncoding, stdout, utf8)
import           Test.Hspec

import           AccountSpecs (accountSpecs)
import           AddressSpecs (addressSpecs)
import           CLI
import           Functions
import           TransactionSpecs (transactionSpecs)
import           Types
import           Util (WalletRef, newWalletRef)
import           WalletSpecs (walletSpecs)

import qualified Data.ByteString.Char8 as B8
import qualified QuickCheckSpecs as QuickCheck


-- | Here we want to run main when the (local) nodes
-- have started.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    CLOptions {..} <- getOptions

    -- TODO (akegalj): run server cluster in haskell, instead of using shell scripts
    -- serverThread <- async (runWalletServer options)

    printT "Starting the integration testing for wallet."

    let serverId = (serverHost, B8.pack $ show serverPort)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- orFail =<< credentialLoadX509 tlsClientCertPath tlsPrivKeyPath
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let baseUrl = BaseUrl Https serverHost serverPort mempty

    let walletClient :: MonadIO m => WalletClient m
        walletClient = liftClient $ mkHttpClient baseUrl manager

    -- Acquire the initial state for the deterministic tests
    wRef <- newWalletRef

    -- NOTE Our own CLI options interfere with `hspec` which parse them for
    -- itself when executed, leading to a VERY unclear message:
    --
    --     cardano-integration-test: unrecognized option `--tls-ca-cert'
    --     Try `cardano-integration-test --help' for more information.
    --
    -- See also: https://github.com/hspec/hspec/issues/135

    let optionsFromAbove = case (testRunnerMatch, testRunnerSeed) of
            (Nothing, Nothing)      -> []
            (Nothing, Just seed)    -> ["--seed", show seed]
            (Just match, Nothing)   -> ["-m", match]
            (Just match, Just seed) -> ["-m", match, "--seed", show seed]

    withArgs optionsFromAbove  $ do
        printT "Starting deterministic tests."
        printT $ "match from options: " <> show testRunnerMatch
        printT $ "seed from options: " <> show testRunnerSeed

        hspec $ deterministicTests wRef walletClient manager

        printT $ "The 'runActionCheck' tests were disabled because they were highly un-reliable."
        when False $ do
            walletState <- initialWalletState walletClient

            printT $ "Initial wallet state: " <> show walletState

            -- some monadic fold or smth similar
            void $ runActionCheck
                walletClient
                walletState
                actionDistribution
  where
    orFail :: MonadFail m => Either String a -> m a
    orFail =
        either (fail . ("Error decoding X509 certificates: " <>)) return

    actionDistribution :: ActionProbabilities
    actionDistribution =
        (PostWallet, Weight 2)
            :| (PostTransaction, Weight 5)
            : fmap (, Weight 1) [minBound .. maxBound]

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
    return WalletState {..}
  where
    fromResp = (either throwM (pure . wrData) =<<)

deterministicTests :: WalletRef -> WalletClient IO -> Manager -> Spec
deterministicTests wref wc manager = do
    accountSpecs wref wc
    addressSpecs wref wc
    walletSpecs wref wc
    transactionSpecs wref wc
    QuickCheck.mkSpec manager
