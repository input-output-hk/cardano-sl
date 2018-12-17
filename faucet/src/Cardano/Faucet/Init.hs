{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Faucet.Init (initEnv) where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Exception.Safe
import           Control.Lens (to)
import           Control.Monad.Except
import           Data.Aeson (FromJSON, eitherDecode)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Int (Int64)
import           Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.IO as Text
import           Data.Text.Lens (packed)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientParams (..), credentialLoadX509FromMemory,
                     defaultParamsClient, onCertificateRequest,
                     onServerCertificate, supportedCiphers)
import           Network.TLS.Extra.Cipher (ciphersuite_strong)
import           Servant.Client.Core (BaseUrl (..), Scheme (..))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (FilePath, takeDirectory)
import           System.IO.Error (IOError, isDoesNotExistError)
import           System.Metrics (Store, createCounter, createGauge)
import qualified System.Metrics.Gauge as Gauge
import           System.Wlog (CanLog, HasLoggerName, LoggerNameBox (..),
                     liftLogIO, logDebug, logError, logInfo, withSublogger)

import           Cardano.Wallet.API.V1.Types (Account (..), Address,
                     AssuranceLevel (NormalAssurance), BackupPhrase (..),
                     ForceNtpCheck (..), NewWallet (..), NodeInfo (..),
                     Payment (..), PaymentDistribution (..),
                     PaymentSource (..), SyncPercentage, V1 (..), Wallet (..),
                     WalletAddress (..), WalletId,
                     WalletOperation (CreateWallet), mkSyncPercentage,
                     txAmount, unV1)
import           Cardano.Wallet.Client (ClientError (..), WalletClient (..),
                     WalletResponse (..), liftClient)
import           Cardano.Wallet.Client.Http (mkHttpClient)
import           Pos.Core (Coin (..))
import           Pos.Util.Mnemonic (Mnemonic, entropyToMnemonic, genEntropy)
import           Universum

import           Cardano.Faucet.Types
import           Cardano.Faucet.Types.Recaptcha

--------------------------------------------------------------------------------
-- | Parses a 'SourceWalletConfig' from a file containing JSON
readSourceWalletConfig :: FilePath -> IO (Either String SourceWalletConfig)
readSourceWalletConfig = readJSON

readJSON :: FromJSON a => FilePath -> IO (Either String a)
readJSON = fmap eitherDecode . BSL.readFile

data CreatedWalletReadError =
    JSONDecodeError String
  | FileNotPresentError
  | FileReadError IOError

readGeneratedWallet :: FilePath -> IO (Either CreatedWalletReadError CreatedWallet)
readGeneratedWallet fp = catch (first JSONDecodeError <$> readJSON fp) $ \e ->
    if isDoesNotExistError e
      then return $ Left FileNotPresentError
      else return $ Left $ FileReadError e
--------------------------------------------------------------------------------
generateBackupPhrase :: IO (Mnemonic 12)
generateBackupPhrase = entropyToMnemonic <$> genEntropy

--------------------------------------------------------------------------------
completelySynced :: SyncPercentage
completelySynced = mkSyncPercentage 100

-- | Looks up the 'SyncPercentage' using 'getNodeInfo' from the 'WalletClient'
getSyncState
    :: (HasLoggerName m, MonadIO m)
    => WalletClient m
    -> m (Either ClientError SyncPercentage)
getSyncState client = do
    r <- getNodeInfo client ForceNtpCheck
    return (nfoSyncProgress . wrData <$> r)

--------------------------------------------------------------------------------

listToEitherT
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => e -> Text -> Text -> [a] -> ExceptT e m a
listToEitherT err errMsg successMsg as = case as of
    [a] -> logInfo successMsg >> return a
    _   -> logError errMsg >> throwError err

runClient
  :: Functor m
  => (err -> e)
  -> m (Either err (WalletResponse r))
  -> ExceptT e m r
runClient err m = ExceptT $ (fmap (first err)) $ fmap (fmap wrData) m

getOneAddress
  :: (HasLoggerName m, CanLog m, MonadIO m)
  => WalletId
  -> Account
  -> ExceptT InitFaucetError m WalletAddress
getOneAddress wId acc =
    let aIdx = accIndex acc
        wIdLog = Text.pack $ show wId
        aIdxLog = Text.pack $ show aIdx
     in listToEitherT
                          (BadAddress wId aIdx)
                          ("Didn't find an address for wallet with ID: "
                                        <> wIdLog
                                        <> " account index: " <> aIdxLog)
                          ("Found a single address for wallet with ID: "
                                        <> wIdLog
                                        <> " account index: " <> aIdxLog)
                          (accAddresses acc)


-- | Creates a new wallet
--
-- Before creating the wallet the 'SyncState' of the node the 'WalletClient' is
-- pointing at checked. If it's less than 100% we wait 5 seconds and try again
createWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => WalletClient m
    -> m (Either InitFaucetError CreatedWallet)
createWallet client = do
    sync <- getSyncState client
    case sync of
        Left err -> do
            logError $ "Error getting sync state: " <> (Text.pack $ show err)
            return . Left $ CouldntReadBalance err
        Right ss | ss >= completelySynced -> do
                       logInfo "Node fully synced, creating wallet"
                       mkWallet
                 | otherwise -> do
                       logInfo $ "Node not fully synced: " <> (Text.pack $ show ss)
                       liftIO $ threadDelay 5000000
                       createWallet client
    where
        mkWallet = do
          phrase <- liftIO generateBackupPhrase
          let w = NewWallet (BackupPhrase phrase) Nothing NormalAssurance "Faucet-Wallet" CreateWallet
          runExceptT $ do
              wId <- walId <$> (runClient WalletCreationError $ postWallet client w)
              let wIdLog = Text.pack $ show wId
              logInfo $ "Created wallet with ID: " <> wIdLog
              accounts <- runClient WalletCreationError $
                            getAccountIndexPaged
                              client
                              wId
                              Nothing
                              Nothing
              acc <- listToEitherT
                          (NoWalletAccounts wId)
                          ("Didn't find an account for wallet with ID: " <> wIdLog)
                          ("Found a single account for wallet with ID: " <> wIdLog)
                          accounts
              let aIdx = accIndex acc
              address <- getOneAddress wId acc
              return (CreatedWallet wId phrase aIdx (unV1 $ addrId address))

--------------------------------------------------------------------------------
-- | Writes a JSON encoded 'CreatedWallet' to the given 'FilePath'
--
-- Creates the parent directory if required
writeCreatedWalletInfo :: FilePath -> CreatedWallet -> IO ()
writeCreatedWalletInfo fp cw = do
    let theDir = takeDirectory fp
    createDirectoryIfMissing True theDir
    Text.writeFile fp $ encodeToLazyText cw

--------------------------------------------------------------------------------
-- | Reads the balance of an existing wallet
--
-- Fails with 'CouldntReadBalance'
readWalletBalance
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => WalletClient m
    -> PaymentSource
    -> ExceptT InitFaucetError m Int64
readWalletBalance client (psWalletId -> wId) = do
    -- lift $ logInfo "Reading initial wallet balance"
    (fromIntegral . getCoin . unV1 . walBalance)
      <$> runClient CouldntReadBalance (getWallet client wId)

--------------------------------------------------------------------------------
-- | Monitor a wallet's balance
--
-- Sets the wallet's balance in the 'Gauge.Gauge' every 5 seconds
monitorWalletBalance
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => FaucetEnv -> m ()
monitorWalletBalance fEnv = do
    let wc = fEnv ^. feWalletClient . to liftClient
        paymentSource = fEnv ^. (feSourceWallet . to cfgToPaymentSource)
        balGauge = fEnv ^. feWalletBalance
    forever $ do
        liftIO $ threadDelay 5000000
        eBal <- runExceptT $ readWalletBalance wc paymentSource
        case eBal of
            Left err -> do
                logError ("Error reading balance: " <> (Text.pack $ show err))
            Right bal -> do
                logDebug ("Read wallet balance: " <> (Text.pack $ show bal))
                liftIO $ Gauge.set balGauge bal

--------------------------------------------------------------------------------
-- | Gets an address out of an existing wallet
--
-- Fails with 'BadAddress'
readReturnAddress
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => WalletClient m
    -> PaymentSource
    -> ExceptT InitFaucetError m (V1 Address)
readReturnAddress client ps = do
    let wId = psWalletId ps
        aIdx = psAccountIndex ps
    lift $ logInfo "Reading return address"
    acc <- runClient (const $ BadAddress wId aIdx) (getAccount client wId aIdx)
    addrId <$> getOneAddress wId acc

-- | Creates the 'IntializedWallet' for a given config
--
-- * In the case of 'Provided' it will use the details of an (existing) wallet by
-- reading from a JSON serialised 'SourceWalletConfig' (and looking up its balance)
-- * If the 'FaucetConfig''s `fcSourceWallet` is 'Generate' a new wallet is
-- created with 'createWallet' and the details are written to the provided
-- 'FilePath'
makeInitializedWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => FaucetConfig
    -> WalletClient m
    -> m (Either InitFaucetError InitializedWallet)
makeInitializedWallet fc client = withSublogger "makeInitializedWallet" $ do
    case (fc ^. fcSourceWallet) of
        Provided fp -> do
            logInfo ("Reading existing wallet details from " <> Text.pack fp)
            srcCfg <- liftIO $ readSourceWalletConfig fp
            case srcCfg of
                Left err -> do
                    logError ( "Error decoding source wallet in read-from: "
                            <> Text.pack err)
                    return $ Left $ SourceWalletParseError err
                Right wc -> do
                    logInfo "Successfully read wallet config"
                    let ps = cfgToPaymentSource wc
                    runExceptT ((InitializedWallet wc)
                                  <$> readWalletBalance client ps
                                  <*> readReturnAddress client ps)
        Generate fp -> do
            logInfo ("Generating wallet details to " <> Text.pack fp <> " (or using existing)")
            eGenWal <- liftIO $ readGeneratedWallet fp
            resp <- case eGenWal of
                Left (JSONDecodeError err) -> do
                    logError ( "Error decoding existing generated wallet: "
                            <> Text.pack err)
                    left $ CreatedWalletReadError err
                Left (FileReadError e) -> do
                    let err = show e
                    logError ( "Error reading file for existing generated wallet: "
                            <> Text.pack err)
                    left $ CreatedWalletReadError err
                Left FileNotPresentError -> do
                    logInfo "File specified in generate-to doesn't exist. Creating wallet"
                    createdWallet <- createWallet client
                    whenRight createdWallet $ \cw -> liftIO $ writeCreatedWalletInfo fp cw
                    return createdWallet
                Right cw -> do
                    logInfo "Wallet read from file specified in generate-to."
                    return $ Right cw
            forM resp $ \(CreatedWallet wallet _phrase accIdx addr) -> do
                    let swc = SourceWalletConfig wallet accIdx Nothing
                    return $  InitializedWallet swc 0 (V1 addr)
    where
        left = return . Left

--------------------------------------------------------------------------------
-- | Process withdrawals requested from the 'feWithdrawalQ'
--
-- On receiving a 'ProcessorPayload' 'postTransaction' is used to send the
-- 'Payment' and the resulting 'Transaction' is wrapped in a 'WithdrawalResult'
-- and put into the provided 'TMVar'
processWithdrawals :: FaucetEnv -> LoggerNameBox IO ()
processWithdrawals fEnv = withSublogger "processWithdrawals" $ forever $ do
    let wc = fEnv ^. feWalletClient
        pmtQ = fEnv ^. feWithdrawalQ
    logInfo "Waiting for next payment"
    (ProcessorPayload pmt tVarResult)<- liftIOA $ TBQ.readTBQueue pmtQ
    logInfo "Processing payment"
    resp <- liftIO $ postTransaction wc pmt
    case resp of
        Left err -> do
            let txtErr = err ^. to show . packed
            logError ("Error sending to " <> (showPmt pmt)
                                          <> " error: "
                                          <> txtErr)
            liftIOA $ putTMVar tVarResult (WithdrawalError txtErr)
        Right withDrawResp -> do
            let txn = wrData withDrawResp
                amount = unV1 $ txAmount txn
            logInfo ((withDrawResp ^. to (show . wrStatus) . packed)
                    <> " withdrew: "
                    <> (amount ^. to show . packed))
            liftIOA $ putTMVar tVarResult (WithdrawalSuccess txn)
    where
      liftIOA = liftIO . atomically
      showPmt = toStrict . encodeToLazyText . pdAddress . NonEmpty.head . pmtDestinations

-- | Creates a 'FaucetEnv' from a given 'FaucetConfig'
--
-- Also sets the 'Gauge.Gauge' for the 'feWalletBalance'
initEnv :: FaucetConfig -> Store -> LoggerNameBox IO FaucetEnv
initEnv fc store = do
    withSublogger "initEnv" $ logInfo "Initializing environment"
    env <- createEnv
    withSublogger "initEnv" $ logInfo "Created environment"
    wdTId <- liftLogIO forkIO $ processWithdrawals env
    withSublogger "initEnv"
        $ logInfo ( "Forked thread for processing withdrawals:"
                 <> show wdTId ^. packed)
    monTId <- liftLogIO forkIO $ monitorWalletBalance env
    withSublogger "initEnv"
        $ logInfo ( "Forked thread for monitoring the wallet balance:"
                 <> show monTId ^. packed)
    return env
  where
    createEnv = withSublogger "init" $ do
      walletBalanceGauge <- liftIO $ createGauge "wallet-balance" store
      feConstruct <- liftIO $ FaucetEnv
        <$> createCounter "total-withdrawn" store
        <*> createCounter "num-withdrawals" store
        <*> pure walletBalanceGauge
      logInfo "Creating Manager"
      manager <- liftIO $ createManager fc
      let url = BaseUrl Https (fc ^. fcWalletApiHost) (fc ^. fcWalletApiPort) ""
          client = mkHttpClient url manager
      logInfo "Initializing wallet"
      initialWallet <- makeInitializedWallet fc (liftClient client)
      pmtQ <- liftIO $ TBQ.newTBQueueIO 10
      mRecaptchaSecret <- liftIO $ traverse readCaptchaSecret (fc ^. fcRecaptchaSecretFile)
      case initialWallet of
          Left err -> do
              logError ( "Error initializing wallet. Exiting: "
                      <> (show err ^. packed))
              throw err
          Right iw -> do
              logInfo ( "Initialised wallet: "
                    <> (iw ^. walletConfig . srcWalletId . to show . packed))
              liftIO $ Gauge.set walletBalanceGauge (iw ^. walletBalance)
              return $ feConstruct
                          store
                          (iw ^. walletConfig)
                          (iw ^. walletReturnAddress)
                          fc
                          client
                          pmtQ
                          mRecaptchaSecret


-- | Makes a http client 'Manager' for communicating with the wallet node
createManager :: FaucetConfig -> IO Manager
createManager fc = do
    pubCert <- BS.readFile (fc ^. fcPubCertFile)
    privKey <- BS.readFile (fc ^. fcPrivKeyFile)
    case credentialLoadX509FromMemory pubCert privKey of
        Left problem -> error $ "Unable to load credentials: " <> (problem ^. packed)
        Right credential ->
            let hooks = def {
                            onCertificateRequest = \_ -> return $ Just credential,
                            -- Only connects to localhost so this isn't required
                            onServerCertificate  = \_ _ _ _ -> return []
                        }
                clientParams = (defaultParamsClient "localhost" "") {
                                   clientHooks = hooks,
                                   clientSupported = def {
                                       supportedCiphers = ciphersuite_strong
                                   }
                               }
                tlsSettings = TLSSettings clientParams
            in
            newManager $ mkManagerSettings tlsSettings Nothing
