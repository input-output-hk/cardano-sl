{-# LANGUAGE NamedFieldPuns #-}

-- | Tx sending functionality in Auxx.

module Command.Tx
       ( sendToAllGenesis
       , send
       , sendTxsFromFile
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue    (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Exception.Safe           (Exception (..), throwString, try)
import           Control.Monad.Except             (runExceptT, throwError)
import qualified Data.ByteString                  as BS
import           Data.List                        ((!!))
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
import           Pos.Client.Txp.Balances          (getOwnUtxoForPk)
import           Pos.Client.Txp.Util              (createTx)
import           Pos.Communication                (SendActions,
                                                   immediateConcurrentConversations,
                                                   submitTx, submitTxRaw)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (BlockVersionData (bvdSlotDuration),
                                                   Timestamp (..), mkCoin)
import           Pos.Core.Configuration           (HasConfiguration,
                                                   genesisBlockVersionData,
                                                   genesisSecretKeys)
import           Pos.Core.Constants               (isDevelopment)
import           Pos.Crypto                       (emptyPassphrase, encToPublic,
                                                   fakeSigner, safeToPublic, toPublic,
                                                   withSafeSigner)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (TxAux, TxOut (..), TxOutAux (..), txaF)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Wallet                       (getSecretKeysPlain)

import           Command.Types                    (SendMode (..),
                                                   SendToAllGenesisParams (..))
import           Mode                             (AuxxMode, CmdCtx (..), getCmdCtx,
                                                   getOwnUtxos)
import           Pos.Auxx                         (makePubKeyAddressAuxx)

----------------------------------------------------------------------------
-- Send to all genesis
----------------------------------------------------------------------------

-- | Count submitted and failed transactions.
--
-- This is used in the benchmarks using send-to-all-genesis
data TxCount = TxCount
    { _txcSubmitted :: !Int
    , _txcFailed    :: !Int
      -- How many threads are still sending transactions.
    , _txcThreads   :: !Int }

addTxSubmit :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxSubmit mvar = modifySharedAtomic mvar (\(TxCount submitted failed sending) -> return (TxCount (submitted + 1) failed sending, ()))

addTxFailed :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxFailed mvar = modifySharedAtomic mvar (\(TxCount submitted failed sending) -> return (TxCount submitted (failed + 1) sending, ()))

sendToAllGenesis
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasGtConfiguration
       )
    => SendActions AuxxMode
    -> SendToAllGenesisParams
    -> AuxxMode ()
sendToAllGenesis sendActions (SendToAllGenesisParams duration conc delay_ sendMode tpsSentFile) = do
    unless (isDevelopment) $
        throwString "sendToAllGenesis works only in development mode"
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
                          utxo <- getOwnUtxoForPk getOwnUtxos $ safeToPublic (fakeSigner key)
                          etx <- createTx mempty utxo (fakeSigner key) txOuts (toPublic key)
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
       , HasGtConfiguration
       )
    => SendActions AuxxMode
    -> Int
    -> NonEmpty TxOut
    -> AuxxMode ()
send sendActions idx outputs = do
    CmdCtx{ccPeers} <- getCmdCtx
    skeys <- getSecretKeysPlain
    let skey = skeys !! idx
        curPk = encToPublic skey
    etx <- withSafeSigner skey (pure emptyPassphrase) $ \mss -> runExceptT $ do
        ss <- mss `whenNothing` throwError (toException $ AuxxException "Invalid passphrase")
        ExceptT $ try $ submitTx
            (immediateConcurrentConversations sendActions ccPeers)
            getOwnUtxos
            mempty
            ss
            (map TxOutAux outputs)
            curPk
    case etx of
        Left err      -> putText $ sformat ("Error: "%stext) (toText $ displayException err)
        Right (tx, _) -> putText $ sformat ("Submitted transaction: "%txaF) tx

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
       , HasGtConfiguration
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
        CmdCtx {ccPeers} <- getCmdCtx
        let submitOne =
                submitTxRaw
                    (immediateConcurrentConversations sendActions ccPeers)
        mapM_ submitOne txAuxes
