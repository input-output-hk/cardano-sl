{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens (_Left)
import           Data.Map (fromList)
import           Data.Traversable (for)
import           Data.X509.File (readSignedObject)
import           Network.HTTP.Client (Manager, ManagerSettings,
                     managerModifyRequest, requestVersion)
import           Network.HTTP.Types (status505)
import           Network.HTTP.Types.Version (http20)
import           System.Environment (withArgs)
import           System.IO (hSetEncoding, stdout, utf8)
import           Test.Hspec

import           AddressSpecs (addressSpecs)
import           CLI
import           Functions
import           TransactionSpecs (transactionSpecs)
import           Types
import           Util (WalletRef, newWalletRef, shouldPrism)
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
    let httpsManagerSettings = mkHttpsManagerSettings serverId caChain clientCredentials
    let baseUrl = BaseUrl Https serverHost serverPort mempty

    manager <- newManager httpsManagerSettings

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

    -- NOTE Our own CLI options interfere with `hspec` which parse them for
    -- itself when executed, leading to a VERY unclear message:
    --
    --     cardano-integration-test: unrecognized option `--tls-ca-cert'
    --     Try `cardano-integration-test --help' for more information.
    --
    -- See also: https://github.com/hspec/hspec/issues/135
    withArgs [] . hspec $ deterministicTests wRef walletClient manager

    -- Basic ping ensuring HTTP/2.0 is disabled
    withArgs [] . hspec =<< mkHTTP20Check baseUrl httpsManagerSettings
  where
    orFail :: MonadFail m => Either String a -> m a
    orFail =
        either (fail . ("Error decoding X509 certificates: " <>)) return

    actionDistribution :: ActionProbabilities
    actionDistribution =
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
    pure WalletState {..}
  where
    fromResp = (either throwM (pure . wrData) =<<)

deterministicTests :: WalletRef -> WalletClient IO -> Manager -> Spec
deterministicTests wref wc manager = do
    addressSpecs wref wc
    walletSpecs wref wc
    transactionSpecs wref wc
    QuickCheck.mkSpec manager


mkHTTP20Check :: BaseUrl -> ManagerSettings -> IO Spec
mkHTTP20Check baseUrl httpsManagerSettings = do
    managerHTTP20 <- newManager (mkHttps20ManagerSettings httpsManagerSettings)
    let walletClientHTTP20 = liftClient $ mkHttpClient baseUrl managerHTTP20
    return $ it "The API isn't accessible via HTTP/2.0" $ do
        eresp <- getWallets walletClientHTTP20
        err <- eresp `shouldPrism` _Left
        let status = case err of
                ClientHttpError (FailureResponse (Response s _ _ _)) -> Just s
                _                                                    -> Nothing
        status `shouldBe` Just status505
  where
    mkHttps20ManagerSettings :: ManagerSettings -> ManagerSettings
    mkHttps20ManagerSettings settings = settings
        { managerModifyRequest = \req -> pure $ req { requestVersion =  http20 }
        }
