{-# LANGUAGE NamedFieldPuns #-}

-- | Tx sending functionality in Auxx.

module Command.Tx
       ( SendToAllGenesisParams (..)
       , sendToAllGenesis
       , send
       , sendTxsFromFile
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue    (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Exception.Safe           (Exception (..), try)
import           Control.Monad.Except             (runExceptT)
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((!!))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Time.Units                  (toMicroseconds)
import           Formatting                       (build, int, sformat, shown, stext, (%))
import           Mockable                         (Mockable, SharedAtomic, SharedAtomicT,
                                                   bracket, concurrently, currentTime,
                                                   delay, forConcurrently,
                                                   modifySharedAtomic, newSharedAtomic)
import           Serokell.Util                    (ms, sec)
import           System.IO                        (BufferMode (LineBuffering), hClose,
                                                   hSetBuffering)
import           System.Random                    (randomRIO)
import           System.Wlog                      (logError, logInfo)

import           Pos.Binary                       (decodeFull)
import           Pos.Client.KeyStorage            (getSecretKeysPlain)
import           Pos.Client.Txp.Balances          (getOwnUtxoForPk)
import           Pos.Client.Txp.Util              (createTx)
import           Pos.Communication                (SendActions,
                                                   immediateConcurrentConversations,
                                                   prepareMTx, submitTxRaw)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (BlockVersionData (bvdSlotDuration),
                                                   IsBootstrapEraAddr (..),
                                                   Timestamp (..), deriveFirstHDAddress,
                                                   makePubKeyAddress, mkCoin)
import           Pos.Core.Configuration           (HasConfiguration,
                                                   genesisBlockVersionData,
                                                   genesisSecretKeys)
import           Pos.Crypto                       (EncryptedSecretKey, emptyPassphrase,
                                                   encToPublic, fakeSigner, safeToPublic,
                                                   toPublic, withSafeSigners)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc.Configuration            (HasSscConfiguration)
import           Pos.Txp                          (TxAux, TxOut (..), TxOutAux (..),
                                                   topsortTxAuxes, txaF)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util.CompileInfo             (HasCompileInfo)
import           Pos.Util.UserSecret              (usWallet, userSecret, wusRootKey)
import           Pos.Util.Util                    (maybeThrow)

import           Lang.Value                       (SendMode (..))
import           Mode                             (AuxxMode, CmdCtx (..), getCmdCtx,
                                                   makePubKeyAddressAuxx)

----------------------------------------------------------------------------
-- Send to all genesis
----------------------------------------------------------------------------

-- | Parameters for 'SendToAllGenesis' command.
data SendToAllGenesisParams = SendToAllGenesisParams
    { stagpDuration    :: !Int
    , stagpConc        :: !Int
    , stagpDelay       :: !Int
    , stagpMode        :: !SendMode
    , stagpTpsSentFile :: !FilePath
    } deriving (Show)

-- | Count submitted and failed transactions.
--
-- This is used in the benchmarks using send-to-all-genesis
data TxCount = TxCount
    { _txcSubmitted :: !Int
    , _txcFailed    :: !Int
      -- How many threads are still sending transactions.
    , _txcThreads   :: !Int }

addTxSubmit :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxSubmit =
    flip modifySharedAtomic
        (\(TxCount submitted failed sending) ->
             pure (TxCount (submitted + 1) failed sending, ()))

addTxFailed :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxFailed =
    flip modifySharedAtomic
        (\(TxCount submitted failed sending) ->
             pure (TxCount submitted (failed + 1) sending, ()))

sendToAllGenesis
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasSscConfiguration
       , HasCompileInfo
       )
    => SendActions AuxxMode
    -> SendToAllGenesisParams
    -> AuxxMode ()
sendToAllGenesis sendActions (SendToAllGenesisParams duration conc delay_ sendMode tpsSentFile) = do
    CmdCtx {ccPeers} <- getCmdCtx
    let nNeighbours = length ccPeers
    let genesisSlotDuration = fromIntegral (toMicroseconds $ bvdSlotDuration genesisBlockVersionData) `div` 1000000 :: Int
        keysToSend  = fromMaybe (error "Genesis secret keys are unknown") genesisSecretKeys
    tpsMVar <- newSharedAtomic $ TxCount 0 0 conc
    startTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
    Mockable.bracket (openFile tpsSentFile WriteMode) (liftIO . hClose) $ \h -> do
        liftIO $ hSetBuffering h LineBuffering
        liftIO . T.hPutStrLn h $ T.intercalate "," [ "slotDuration=" <> show genesisSlotDuration
                                                   , "sendMode=" <> show sendMode
                                                   , "conc=" <> show conc
                                                   , "startTime=" <> startTime
                                                   , "delay=" <> show delay_ ]
        liftIO $ T.hPutStrLn h "time,txCount,txType"
        txQueue <- atomically $ newTQueue
        -- prepare a queue with all transactions
        logInfo $ sformat ("Found "%shown%" keys in the genesis block.") (length keysToSend)
        forM_ (zip keysToSend [0..]) $ \(secretKey, n) -> do
            outAddr <- makePubKeyAddressAuxx (toPublic secretKey)
            let val1 = mkCoin 1
                txOut1 = TxOut {
                    txOutAddress = outAddr,
                    txOutValue = val1
                    }
                txOuts = TxOutAux txOut1 :| []
            neighbours <- case sendMode of
                SendNeighbours -> return ccPeers
                SendRoundRobin -> return [ccPeers !! (n `mod` nNeighbours)]
                SendRandom -> do
                    i <- liftIO $ randomRIO (0, nNeighbours - 1)
                    return [ccPeers !! i]
            atomically $ writeTQueue txQueue (secretKey, txOuts, neighbours)

            -- every <slotDuration> seconds, write the number of sent and failed transactions to a CSV file.
        let writeTPS :: AuxxMode ()
            writeTPS = do
                delay (sec genesisSlotDuration)
                curTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
                finished <- modifySharedAtomic tpsMVar $ \(TxCount submitted failed sending) -> do
                    -- CSV is formatted like this:
                    -- time,txCount,txType
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [curTime, show $ submitted, "submitted"]
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [curTime, show $ failed, "failed"]
                    return (TxCount 0 0 sending, sending <= 0)
                if finished
                then logInfo "Finished writing TPS samples."
                else writeTPS
            -- Repeatedly take transactions from the queue and send them.
            -- Do this n times.
            sendTxs :: Int -> AuxxMode ()
            sendTxs n
                | n <= 0 = do
                      logInfo "All done sending transactions on this thread."
                      modifySharedAtomic tpsMVar $ \(TxCount submitted failed sending) ->
                          return (TxCount submitted failed (sending - 1), ())
                | otherwise = (atomically $ tryReadTQueue txQueue) >>= \case
                      Just (key, txOuts, neighbours) -> do
                          utxo <- getOwnUtxoForPk $ safeToPublic (fakeSigner key)
                          etx <- createTx utxo (fakeSigner key) txOuts (toPublic key)
                          case etx of
                              Left err -> do
                                  addTxFailed tpsMVar
                                  logError (sformat ("Error: "%build%" while trying to send to "%shown) err neighbours)
                              Right (tx, _) -> do
                                  res <- submitTxRaw (immediateConcurrentConversations sendActions neighbours) tx
                                  addTxSubmit tpsMVar
                                  logInfo $ if res
                                      then sformat ("Submitted transaction: "%txaF%" to "%shown) tx neighbours
                                      else sformat ("Applied transaction "%txaF%", however no neighbour applied it") tx
                          delay $ ms delay_
                          logInfo "Continuing to send transactions."
                          sendTxs (n - 1)
                      Nothing -> logInfo "No more transactions in the queue."
            sendTxsConcurrently n = void $ forConcurrently [1..conc] (const (sendTxs n))
        -- Send transactions while concurrently writing the TPS numbers every
        -- slot duration. The 'writeTPS' action takes care to *always* write
        -- after every slot duration, even if it is killed, so as to
        -- guarantee that we don't miss any numbers.
        void $ concurrently writeTPS (sendTxsConcurrently duration)

----------------------------------------------------------------------------
-- Casual sending
----------------------------------------------------------------------------

newtype AuxxException = AuxxException Text
  deriving (Show)

instance Exception AuxxException

send
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasSscConfiguration
       , HasCompileInfo
       )
    => SendActions AuxxMode
    -> Int
    -> NonEmpty TxOut
    -> AuxxMode ()
send sendActions idx outputs = do
    CmdCtx{ccPeers} <- getCmdCtx
    skey <- takeSecret
    let curPk = encToPublic skey
    let plainAddresses = map (flip makePubKeyAddress curPk . IsBootstrapEraAddr) [False, True]
    let (hdAddresses, hdSecrets) = unzip $ map
            (\ibea -> fromMaybe (error "send: pass mismatch") $
                    deriveFirstHDAddress (IsBootstrapEraAddr ibea) emptyPassphrase skey) [False, True]
    let allAddresses = hdAddresses ++ plainAddresses
    let allSecrets = hdSecrets ++ [skey, skey]
    etx <- withSafeSigners allSecrets (pure emptyPassphrase) $ \signers -> runExceptT @AuxxException $ do
        let addrSig = HM.fromList $ zip allAddresses signers
        let getSigner = fromMaybe (error "Couldn't get SafeSigner") . flip HM.lookup addrSig
        -- BE CAREFUL: We create remain address using our pk, wallet doesn't show such addresses
        (txAux,_) <- lift $ prepareMTx getSigner (NE.fromList allAddresses) (map TxOutAux outputs) curPk
        txAux <$ (ExceptT $ try $ submitTxRaw (immediateConcurrentConversations sendActions ccPeers) txAux)
    case etx of
        Left err -> logError $ sformat ("Error: "%stext) (toText $ displayException err)
        Right tx -> logInfo $ sformat ("Submitted transaction: "%txaF) tx
  where
    takeSecret :: AuxxMode EncryptedSecretKey
    takeSecret
        | idx == -1 = do
            _userSecret <- view userSecret >>= atomically . readTVar
            pure $ maybe (error "Unknown wallet address") (^. wusRootKey) (_userSecret ^. usWallet)
        | otherwise = (!! idx) <$> getSecretKeysPlain

----------------------------------------------------------------------------
-- Send from file
----------------------------------------------------------------------------

-- | Read transactions from the given file (ideally generated by
-- 'rollbackAndDump') and submit them to the network.
sendTxsFromFile
    :: ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       , HasSscConfiguration
       , HasCompileInfo
       )
    => SendActions AuxxMode
    -> FilePath
    -> AuxxMode ()
sendTxsFromFile sendActions txsFile = do
    liftIO (BS.readFile txsFile) <&> decodeFull >>= \case
        Left err -> throwM (AuxxException err)
        Right txs -> sendTxs txs
  where
    sendTxs :: [TxAux] -> AuxxMode ()
    sendTxs txAuxes = do
        logInfo $
            sformat
                ("Going to send "%int%" transactions one-by-one")
                (length txAuxes)
        sortedTxAuxes <-
            maybeThrow
                (AuxxException "txs form a cycle")
                (topsortTxAuxes txAuxes)
        CmdCtx {ccPeers} <- getCmdCtx
        let submitOne =
                submitTxRaw
                    (immediateConcurrentConversations sendActions ccPeers)
        mapM_ submitOne sortedTxAuxes
