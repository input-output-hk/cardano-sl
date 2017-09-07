{-# LANGUAGE NamedFieldPuns #-}

-- | Tx sending functionality in Rubbish.

module Tx
       ( sendToAllGenesis
       , send
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Exception.Safe        (Exception (..), try)
import           Control.Monad.Except          (runExceptT, throwError)
import           Data.List                     ((!!))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time.Units               (toMicroseconds)
import           Formatting                    (build, sformat, shown, stext, (%))
import           Mockable                      (Mockable, SharedAtomic, SharedAtomicT,
                                                bracket, concurrently, currentTime, delay,
                                                forConcurrently, modifySharedAtomic,
                                                newSharedAtomic)
import           Serokell.Util                 (ms, sec)
import           System.IO                     (BufferMode (LineBuffering), hClose,
                                                hSetBuffering)
import           System.Random                 (randomRIO)
import           System.Wlog                   (logError, logInfo)

import           Pos.Client.Txp.Balances       (getOwnUtxoForPk)
import           Pos.Client.Txp.Util           (createTx)
import           Pos.Communication             (SendActions,
                                                immediateConcurrentConversations,
                                                submitTx, submitTxRaw)
import           Pos.Constants                 (genesisSlotDuration)
import           Pos.Core                      (Timestamp (..), mkCoin)
import           Pos.Core.Context              (HasCoreConstants)
import           Pos.Crypto                    (emptyPassphrase, encToPublic, fakeSigner,
                                                safeToPublic, toPublic, withSafeSigner)
import           Pos.Genesis                   (balanceDistribution)
import           Pos.Rubbish                   (LightWalletMode, makePubKeyAddressRubbish)
import           Pos.Txp                       (TxOut (..), TxOutAux (..), txaF)
import           Pos.Wallet                    (getSecretKeys)

import           Command                       (CmdCtx (..), SendMode (..),
                                                SendToAllGenesisParams (..))

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

sendToAllGenesis :: HasCoreConstants => SendActions LightWalletMode -> SendToAllGenesisParams -> CmdCtx -> LightWalletMode ()
sendToAllGenesis sendActions (SendToAllGenesisParams duration conc delay_ sendMode tpsSentFile) CmdCtx{..} = do
    let nNeighbours = length na
    let slotDuration = fromIntegral (toMicroseconds genesisSlotDuration) `div` 1000000 :: Int
        keysToSend = zip skeys (balanceDistribution genesisBalanceDistr)
    tpsMVar <- newSharedAtomic $ TxCount 0 0 conc
    startTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
    Mockable.bracket (openFile tpsSentFile WriteMode) (liftIO . hClose) $ \h -> do
        liftIO $ hSetBuffering h LineBuffering
        liftIO . T.hPutStrLn h $ T.intercalate "," [ "slotDuration=" <> show slotDuration
                                                   , "sendMode=" <> show sendMode
                                                   , "conc=" <> show conc
                                                   , "startTime=" <> startTime
                                                   , "delay=" <> show delay_ ]
        liftIO $ T.hPutStrLn h "time,txCount,txType"
        txQueue <- atomically $ newTQueue
        -- prepare a queue with all transactions
        logInfo $ sformat ("Found "%shown%" keys in the genesis block.") (length keysToSend)
        -- Light wallet doesn't know current slot, so let's assume
        -- it's 0-th epoch. It's enough for our current needs.
        forM_ (zip keysToSend [0..]) $ \((key, _balance), n) -> do
            outAddr <- makePubKeyAddressRubbish (toPublic key)
            let val1 = mkCoin 1
                txOut1 = TxOut {
                    txOutAddress = outAddr,
                    txOutValue = val1
                    }
                txOuts = TxOutAux txOut1 :| []
            neighbours <- case sendMode of
                SendNeighbours -> return na
                SendRoundRobin -> return [na !! (n `mod` nNeighbours)]
                SendRandom -> do
                    i <- liftIO $ randomRIO (0, nNeighbours - 1)
                    return [na !! i]
            atomically $ writeTQueue txQueue (key, txOuts, neighbours)

            -- every <slotDuration> seconds, write the number of sent and failed transactions to a CSV file.
        let writeTPS :: LightWalletMode ()
            writeTPS = do
                delay (sec slotDuration)
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
            sendTxs :: Int -> LightWalletMode ()
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

newtype LWalletException = LWalletException Text
  deriving (Show)

instance Exception LWalletException

send ::
       HasCoreConstants
    => SendActions LightWalletMode
    -> Int
    -> NonEmpty TxOut
    -> CmdCtx
    -> LightWalletMode ()
send sendActions idx outputs CmdCtx{na} = do
    skeys <- getSecretKeys
    let skey = skeys !! idx
        curPk = encToPublic skey
    etx <- withSafeSigner skey (pure emptyPassphrase) $ \mss -> runExceptT $ do
        ss <- mss `whenNothing` throwError (toException $ LWalletException "Invalid passphrase")
        ExceptT $ try $ submitTx
            (immediateConcurrentConversations sendActions na)
            ss
            (map TxOutAux outputs)
            curPk
    case etx of
        Left err      -> putText $ sformat ("Error: "%stext) (toText $ displayException err)
        Right (tx, _) -> putText $ sformat ("Submitted transaction: "%txaF) tx
