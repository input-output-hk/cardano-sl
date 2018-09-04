{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tx sending functionality in Auxx.

module Command.Tx
       ( SendToAllGenesisParams (..)
       , sendToAllGenesis
       , send
       , sendTxsFromFile
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Exception.Safe (Exception (..), try)
import           Control.Monad (when)
import           Control.Monad.Except (runExceptT)
import           Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Units (Microsecond, fromMicroseconds, toMicroseconds)
import           Formatting (build, int, sformat, shown, stext, (%))
import           Mockable (Mockable, SharedAtomic, SharedAtomicT, concurrently, currentTime, delay,
                           forConcurrently, modifySharedAtomic, newSharedAtomic)
import           System.Environment (lookupEnv)
import           System.IO (BufferMode (LineBuffering), hClose, hSetBuffering)
import           System.Wlog (logError, logInfo)

import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Client.Txp.Balances (getOwnUtxoForPk)
import           Pos.Client.Txp.Network (prepareMTx, submitTxRaw)
import           Pos.Client.Txp.Util (createTx)
import           Pos.Core (BlockVersionData (bvdSlotDuration), IsBootstrapEraAddr (..),
                           Timestamp (..), deriveFirstHDAddress, makePubKeyAddress, mkCoin)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisSecretKeys)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Core.Txp (TxAux (..), TxIn (TxInUtxo), TxOut (..), TxOutAux (..), txaF)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic, emptyPassphrase, encToPublic,
                             fakeSigner, hash, safeToPublic, toPublic, withSafeSigners)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Txp (topsortTxAuxes)
import           Pos.Util.UserSecret (usWallet, userSecret, wusRootKey)
import           Pos.Util.Util (maybeThrow)

import           Mode (MonadAuxxMode, makePubKeyAddressAuxx)

----------------------------------------------------------------------------
-- Send to all genesis
----------------------------------------------------------------------------

-- | Parameters for 'SendToAllGenesis' command.
data SendToAllGenesisParams = SendToAllGenesisParams
    { stagpGenesisTxsPerThread :: !Int
    , stagpTxsPerThread        :: !Int
    , stagpConc                :: !Int
    , stagpDelay               :: !Int
    , stagpTpsSentFile         :: !FilePath
    } deriving (Show)

-- | Count submitted transactions.
--
-- This is used in the benchmarks using send-to-all-genesis
data TxCount = TxCount
    { _txcSubmitted :: !Int
      -- How many threads are still sending transactions.
    , _txcThreads   :: !Int }

addTxSubmit :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxSubmit =
    flip modifySharedAtomic
        (\(TxCount submitted sending) ->
             pure (TxCount (submitted + 1) sending, ()))

sendToAllGenesis
    :: forall m. MonadAuxxMode m
    => ProtocolMagic
    -> Diffusion m
    -> SendToAllGenesisParams
    -> m ()
sendToAllGenesis pm diffusion (SendToAllGenesisParams genesisTxsPerThread txsPerThread conc delay_ tpsSentFile) = do
    let genesisSlotDuration = fromIntegral (toMicroseconds $ bvdSlotDuration genesisBlockVersionData) `div` 1000000 :: Int
        keysToSend  = fromMaybe (error "Genesis secret keys are unknown") genesisSecretKeys
    tpsMVar <- newSharedAtomic $ TxCount 0 conc
    startTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
    bracket (openFile tpsSentFile WriteMode) (liftIO . hClose) $ \h -> do
        liftIO $ hSetBuffering h LineBuffering
        liftIO . T.hPutStrLn h $ T.intercalate "," [ "slotDuration=" <> show genesisSlotDuration
                                                   , "conc=" <> show conc
                                                   , "startTime=" <> startTime
                                                   , "delay=" <> show delay_ ]
        liftIO $ T.hPutStrLn h "time,txCount,txType"
        -- prepare a queue for the transactions to be send
        -- and a queue with necessary data for the creation of txs whose inputs
        -- don't belong in genesis block
        txQueue            <- atomically $ newTQueue
        txPreparationQueue <- atomically $ newTQueue
        logInfo $ sformat ("Found "%shown%" keys in the genesis block.") (length keysToSend)
        startAtTxt <- liftIO $ lookupEnv "AUXX_START_AT"
        let startAt = fromMaybe 0 . readMaybe . fromMaybe "" $ startAtTxt :: Int
        let nm = makeNetworkMagic pm
        -- construct a transaction, and add it to the queue
        let addTx secretKey = do
                let signer = fakeSigner secretKey
                    publicKey = toPublic secretKey
                -- construct transaction output
                outAddr <- makePubKeyAddressAuxx nm publicKey
                let txOut1 = TxOut {
                    txOutAddress = outAddr,
                    txOutValue = mkCoin 1
                    }
                    txOuts = TxOutAux txOut1 :| []
                utxo <- getOwnUtxoForPk nm $ safeToPublic signer
                etx <- createTx pm mempty utxo signer txOuts publicKey
                case etx of
                    Left err -> logError (sformat ("Error: "%build%" while trying to contruct tx") err)
                    Right (tx, _) -> do
                        atomically $ writeTQueue txQueue tx
                        atomically $ writeTQueue txPreparationQueue (tx, txOut1, secretKey)
        let genesisTxs  = conc * genesisTxsPerThread
            nTxs        = conc * txsPerThread
            genesisKeys = take genesisTxs (drop startAt keysToSend)
            -- Construct transactions whose inputs does not belong in genesis
            -- block. Send as many coins as were received in the previous round
            -- back to yourself.
            prepareTxs :: Int -> m ()
            prepareTxs n
                | n <= 0 = return ()
                | otherwise = (atomically $ tryReadTQueue txPreparationQueue) >>= \case
                    Just (tx, txOut1', senderKey) -> do
                        let txInp = TxInUtxo (hash (taTx tx)) 0
                            utxo' = M.fromList [(txInp, TxOutAux txOut1')]
                            txOuts2 = TxOutAux txOut1' :| []
                        -- It is expected that the output from the previously sent transaction is
                        -- included in the UTxO by the time this transaction will actually be sent.
                        etx' <- createTx pm mempty utxo' (fakeSigner senderKey) txOuts2 (toPublic senderKey)
                        case etx' of
                            Left err -> logError (sformat ("Error: "%build%" while trying to contruct tx") err)
                            Right (tx', _) -> do
                                atomically $ writeTQueue txQueue tx'
                                -- add to preparation queue one more time data
                                -- necessary to construct one more transaction
                                -- with the same sender
                                when (n > genesisTxs) $
                                    atomically $ writeTQueue txPreparationQueue (tx', txOut1', senderKey)
                        prepareTxs $ n - 1
                    Nothing -> logInfo "No more txOuts in the queue."
            -- every <slotDuration> seconds, write the number of sent transactions to a CSV file.
        let writeTPS :: m ()
            writeTPS = do
                delay (fromMicroseconds . fromIntegral . (*) 1000000 $ genesisSlotDuration :: Microsecond)
                curTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
                finished <- modifySharedAtomic tpsMVar $ \(TxCount submitted sending) -> do
                    -- CSV is formatted like this:
                    -- time,txCount,txType
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [curTime, show $ submitted, "submitted"]
                    return (TxCount 0 sending, sending <= 0)
                if finished
                    then logInfo "Finished writing TPS samples."
                    else writeTPS
            -- Repeatedly take transactions from the queue and send them.
            -- Do this n times.
            sendTxs :: Int -> m ()
            sendTxs n
                | n <= 0 = do
                      logInfo "All done sending transactions on this thread."
                      modifySharedAtomic tpsMVar $ \(TxCount submitted sending) ->
                          return (TxCount submitted (sending - 1), ())
                | otherwise = (atomically $ tryReadTQueue txQueue) >>= \case
                      Just tx -> do
                          res <- submitTxRaw diffusion tx
                          addTxSubmit tpsMVar
                          logInfo $ if res
                                    then sformat ("Submitted transaction: "%txaF) tx
                                    else sformat ("Applied transaction "%txaF%", however no neighbour applied it") tx
                          delay $ (fromMicroseconds . fromIntegral . (*) 1000 $ delay_ :: Microsecond)
                          logInfo "Continuing to send transactions."
                          sendTxs (n - 1)
                      Nothing -> do
                          logInfo "No more transactions in the queue."
                          sendTxs 0

            sendTxsConcurrently n = void $ forConcurrently [1..conc] (const (sendTxs n))
        -- pre construct the transactions that send funds from genesis.
        -- Otherwise, we'll be CPU bound and will not achieve high transaction
        -- rates. If we pre construct all the transactions, the
        -- startup time will be quite long.
        forM_ genesisKeys addTx
        -- Send transactions while concurrently writing the TPS numbers every
        -- slot duration. The 'writeTPS' action takes care to *always* write
        -- after every slot duration, even if it is killed, so as to
        -- guarantee that we don't miss any numbers.
        --
        -- While we're sending, we're constructing the transactions
        -- that don't send coins from genesis and send 1 coin back to
        -- themselves over and over.
        void $
            concurrently (prepareTxs $ nTxs - genesisTxs) $
            concurrently writeTPS (sendTxsConcurrently txsPerThread)

----------------------------------------------------------------------------
-- Casual sending
----------------------------------------------------------------------------

newtype AuxxException = AuxxException Text
  deriving (Show)

instance Exception AuxxException

send
    :: forall m. MonadAuxxMode m
    => ProtocolMagic
    -> Diffusion m
    -> Int
    -> NonEmpty TxOut
    -> m ()
send pm diffusion idx outputs = do
    skey <- takeSecret
    let nm = makeNetworkMagic pm
    let curPk = encToPublic skey
    let plainAddresses = map (flip (makePubKeyAddress nm) curPk . IsBootstrapEraAddr) [False, True]
    let (hdAddresses, hdSecrets) = unzip $ map
            (\ibea -> fromMaybe (error "send: pass mismatch") $
                    deriveFirstHDAddress nm (IsBootstrapEraAddr ibea) emptyPassphrase skey) [False, True]
    let allAddresses = hdAddresses ++ plainAddresses
    let allSecrets = hdSecrets ++ [skey, skey]
    etx <- withSafeSigners allSecrets (pure emptyPassphrase) $ \signers -> runExceptT @AuxxException $ do
        let addrSig = HM.fromList $ zip allAddresses signers
        let getSigner addr = HM.lookup addr addrSig
        -- BE CAREFUL: We create remain address using our pk, wallet doesn't show such addresses
        (txAux,_) <- lift $ prepareMTx pm getSigner mempty def (NE.fromList allAddresses) (map TxOutAux outputs) curPk
        txAux <$ (ExceptT $ try $ submitTxRaw diffusion txAux)
    case etx of
        Left err -> logError $ sformat ("Error: "%stext) (toText $ displayException err)
        Right tx -> logInfo $ sformat ("Submitted transaction: "%txaF) tx
  where
    takeSecret :: m EncryptedSecretKey
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
    :: forall m. MonadAuxxMode m
    => Diffusion m
    -> FilePath
    -> m ()
sendTxsFromFile diffusion txsFile = do
    liftIO (BS.readFile txsFile) <&> eitherDecodeStrict >>= \case
        Left err -> throwM (AuxxException $ toText err)
        Right txs -> sendTxs txs
  where
    sendTxs :: [TxAux] -> m ()
    sendTxs txAuxes = do
        logInfo $
            sformat
                ("Going to send "%int%" transactions one-by-one")
                (length txAuxes)
        sortedTxAuxes <-
            maybeThrow
                (AuxxException "txs form a cycle")
                (topsortTxAuxes txAuxes)
        let submitOne = submitTxRaw diffusion
        mapM_ submitOne sortedTxAuxes
