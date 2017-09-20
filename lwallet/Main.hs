{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# LANGUAGE RankNTypes     #-}

module Main
       ( main
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue    (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Monad.Catch              (Exception (..), try)
import           Control.Monad.Except             (runExceptT, throwError)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Base58           (bitcoinAlphabet, encodeBase58)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((!!))
import qualified Data.Set                         as S (fromList)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Time.Units                  (convertUnit)
import           Formatting                       (build, int, sformat, shown, stext,
                                                   string, (%))
import           Mockable                         (Mockable, Production, SharedAtomic,
                                                   SharedAtomicT, bracket, concurrently,
                                                   currentTime, delay, forConcurrently,
                                                   modifySharedAtomic, newSharedAtomic,
                                                   runProduction)
import           NeatInterpolation                (text)
import           Network.Transport.Abstract       (Transport, hoistTransport)
import           System.IO                        (BufferMode (LineBuffering), hClose,
                                                   hFlush, hSetBuffering, stdout)
import           System.Wlog                      (logDebug, logError, logInfo)
#if !(defined(mingw32_HOST_OS))
import           System.Exit                      (ExitCode (ExitSuccess))
import           System.Posix.Process             (exitImmediately)
#endif
import           Serokell.Util                    (ms, sec)

import           Pos.Binary                       (Raw, serialize')
import qualified Pos.Client.CLI                   as CLI
import           Pos.Client.Txp.Balances          (getOwnUtxoForPk)
import           Pos.Client.Txp.Util              (createTx)
import           Pos.Communication                (NodeId, OutSpecs, SendActions, Worker,
                                                   WorkerSpec, dataFlow, delegationRelays,
                                                   immediateConcurrentConversations,
                                                   relayPropagateOut, submitTx,
                                                   submitTxRaw, submitUpdateProposal,
                                                   submitVote, txRelays, usRelays, worker)
import           Pos.Core                         (addressHash, coinF, bvdSlotDuration)
import           Pos.Core.Configuration           (genesisBlockVersionData,
                                                   genesisSecretKeys)
import           Pos.Core.Constants               (isDevelopment)
import           Pos.Core.Address                 (makeAddress)
import           Pos.Core.Types                   (AddrAttributes (..),
                                                   AddrSpendingData (..), Timestamp (..),
                                                   mkCoin)
import           Pos.Crypto                       (Hash, SecretKey, SignTag (SignUSVote),
                                                   emptyPassphrase, encToPublic,
                                                   fakeSigner, fullPublicKeyHexF, hash,
                                                   hashHexF, noPassEncrypt, safeCreatePsk,
                                                   safeSign, safeToPublic, toPublic,
                                                   unsafeHash, withSafeSigner)
import           Pos.Data.Attributes              (mkAttributes)
import           Pos.Launcher                     (BaseParams (..), LoggingParams (..),
                                                   bracketTransport, loggerBracket,
                                                   HasConfigurations, withConfigurations)
import           Pos.Network.Types                (MsgType (..), Origin (..))
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Txp                          (TxOut (..), TxOutAux (..), txaF,
                                                   genesisUtxo, unGenesisUtxo)
import           Pos.Update                       (BlockVersionModifier (..),
                                                   SystemTag (..), UpdateData (..),
                                                   UpdateVote (..), mkUpdateProposalWSign)
import           Pos.Util.UserSecret              (readUserSecret, usKeys)
import           Pos.Util.Util                    (powerLift)
import           Pos.Wallet                       (addSecretKey, getBalance,
                                                   getSecretKeys)
import           Pos.Wallet.Light                 (LightWalletMode, WalletParams (..),
                                                   makePubKeyAddressLWallet,
                                                   runWalletStaticPeers)
import           Pos.WorkMode                     (RealMode, RealModeContext)

import           Command                          (Command (..), ProposeUpdateSystem (..),
                                                   SendMode (..), parseCommand)
import qualified Network.Transport.TCP            as TCP (TCPAddr (..))
import           System.Random                    (randomRIO)
import           WalletOptions                    (WalletAction (..), WalletOptions (..),
                                                   getWalletOptions)

import           Node.Conversation                (ConversationActions (..))
import           Node.Message.Class               (Message (..))
import           Pos.Communication.Types.Protocol (Conversation (..), SendActions (..),
                                                   enqueueMsg, withConnectionTo)
import           System.Wlog.CanLog

data CmdCtx = CmdCtx
    { skeys               :: [SecretKey]
    , na                  :: [NodeId]
    }

helpMsg :: Text
helpMsg = [text|
Avaliable commands:
   balance <address>              -- check balance on given address (may be any address)
   send <N> [<address> <coins>]+  -- create and send transaction with given outputs
                                     from own address #N
   send-to-all-genesis <duration> <conc> <delay> <sendmode> <csvfile>
                                  -- create and send transactions from all genesis addresses for <duration>
                                     seconds, delay in ms.  conc is the number of threads that send
                                     transactions concurrently. sendmode can be one of "neighbours",
                                     "round-robin", and "send-random".
   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base16) and
                                     decision, from own address #N
   propose-update <N> <block ver> <script ver> <slot duration> <max block size> <software ver> <propose_file>?
                                  -- propose an update with given versions and other data
                                     with one positive vote for it, from own address #N
   listaddr                       -- list own addresses
   delegate-light <N> <M> <eStart> <eEnd>?
                                  -- delegate secret key #N to pk <M> light version (M is encoded in base58),
                                     where eStart is cert start epoch, eEnd -- expire epoch
   delegate-heavy <N> <M> <e>     -- delegate secret key #N to pk <M> heavyweight (M is encoded in base58),
                                     e is current epoch.
   add-key-pool <N>               -- add key from intial pool
   add-key <file>                 -- add key from file

   addr-distr <N> boot
   addr-distr <N> [<M>:<coinPortion>]+
                                  -- print the address for pk <N> (encoded in base58) with the specified distribution,
                                  -- where <M> is stakeholder id (pk hash), and the coin portion can be a coefficient
                                  -- in [0..1] or a percentage (ex. 42%)

   help                           -- show this message
   quit                           -- shutdown node wallet
|]

newtype LWalletException = LWalletException Text
  deriving (Show)

instance Exception LWalletException

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

runCmd :: HasConfigurations => SendActions LightWalletMode -> Command -> CmdCtx -> LightWalletMode ()
runCmd _ (Balance addr) _ =
    getBalance addr >>=
    putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) CmdCtx{na} = do
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
runCmd sendActions (SendToAllGenesis duration conc delay_ sendMode tpsSentFile) CmdCtx{..} = do
    let nNeighbours = length na
    let slotDuration = bvdSlotDuration genesisBlockVersionData
        keysToSend = fromMaybe (error "Must have secret keys") genesisSecretKeys
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
        forM_ (zip keysToSend [0..]) $ \(key, n) -> do
            outAddr <- makePubKeyAddressLWallet (toPublic key)
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
                delay slotDuration
                currentTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
                finished <- modifySharedAtomic tpsMVar $ \(TxCount submitted failed sending) -> do
                    -- CSV is formatted like this:
                    -- time,txCount,txType
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [currentTime, show $ submitted, "submitted"]
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [currentTime, show $ failed, "failed"]
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
runCmd sendActions v@(Vote idx decision upid) CmdCtx{na} = do
    logDebug $ "Submitting a vote :" <> show v
    skey <- (!! idx) <$> getSecretKeys
    msignature <- withSafeSigner skey (pure emptyPassphrase) $ mapM $
                        \ss -> pure $ safeSign SignUSVote ss (upid, decision)
    case msignature of
        Nothing -> putText "Invalid passphrase"
        Just signature -> do
            let voteUpd = UpdateVote
                    { uvKey        = encToPublic skey
                    , uvProposalId = upid
                    , uvDecision   = decision
                    , uvSignature  = signature
                }
            if null na
                then putText "Error: no addresses specified"
                else do
                    submitVote (immediateConcurrentConversations sendActions na) voteUpd
                    putText "Submitted vote"
runCmd sendActions ProposeUpdate{..} CmdCtx{na} = do
    logDebug "Proposing update..."
    skey <- (!! puIdx) <$> getSecretKeys
    let bvm =
            BlockVersionModifier
            { bvmScriptVersion     = Just puScriptVersion
            , bvmSlotDuration      = Just (convertUnit (sec puSlotDurationSec))
            , bvmMaxBlockSize      = Just puMaxBlockSize
            , bvmMaxHeaderSize     = Nothing
            , bvmMaxTxSize         = Nothing
            , bvmMaxProposalSize   = Nothing
            , bvmMpcThd            = Nothing
            , bvmHeavyDelThd       = Nothing
            , bvmUpdateVoteThd     = Nothing
            , bvmUpdateProposalThd = Nothing
            , bvmUpdateImplicit    = Nothing
            , bvmSoftforkRule      = Nothing
            , bvmTxFeePolicy       = Nothing
            , bvmUnlockStakeEpoch  = Nothing
            }
    updateData <- mapM updateDataElement puUpdates
    let udata = HM.fromList updateData
    let whenCantCreate = error . mappend "Failed to create update proposal: "
    withSafeSigner skey (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let updateProposal = either whenCantCreate identity $
                    mkUpdateProposalWSign
                        puBlockVersion
                        bvm
                        puSoftwareVersion
                        udata
                        (mkAttributes ())
                        ss
            if null na
                then putText "Error: no addresses specified"
                else do
                    submitUpdateProposal (immediateConcurrentConversations sendActions na) ss updateProposal
                    let id = hash updateProposal
                    putText $
                      sformat ("Update proposal submitted, upId: "%hashHexF) id
runCmd _ Help _ = putText helpMsg
runCmd _ ListAddresses _ = do
   addrs <- map encToPublic <$> getSecretKeys
    -- Light wallet doesn't know current slot, so let's assume
    -- it's 0-th epoch. It's enough for our current needs.
   putText "Available addresses:"
   for_ (zip [0 :: Int ..] addrs) $ \(i, pk) -> do
       addr <- makePubKeyAddressLWallet pk
       putText $ sformat ("    #"%int%":   addr:      "%build%"\n"%
                          "          pk base58: "%stext%"\n"%
                          "          pk hex:    "%fullPublicKeyHexF%"\n"%
                          "          pk hash:   "%hashHexF)
                    i addr (toBase58Text pk) pk (addressHash pk)
  where
    toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . serialize'
runCmd sendActions (DelegateLight i delegatePk startEpoch lastEpochM) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreatePsk ss delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
          dataFlow "pskLight" (immediateConcurrentConversations sendActions na) (MsgTransaction OriginSender) psk
   putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i delegatePk curEpoch) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreatePsk ss delegatePk curEpoch
          dataFlow "pskHeavy" (immediateConcurrentConversations sendActions na) (MsgTransaction OriginSender) psk
   putText "Sent heavyweight cert"
runCmd _ (AddKeyFromPool i) CmdCtx{..} = do
   let key = skeys !! i
   addSecretKey $ noPassEncrypt key
runCmd _ (AddKeyFromFile f) _ = do
    secret <- readUserSecret f
    mapM_ addSecretKey $ secret ^. usKeys
runCmd _ (AddrDistr pk asd) _ = do
    putText $ pretty addr
  where
    addr = makeAddress (PubKeyASD pk) (AddrAttributes Nothing asd)
runCmd _ Quit _ = pure ()

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: MonadIO m => Maybe FilePath -> m (Hash Raw)
hashFile Nothing  = pure dummyHash
hashFile (Just filename) = do
    fileData <- liftIO $ BS.readFile filename
    let h = unsafeHash fileData
    putText $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h

updateDataElement :: MonadIO m => ProposeUpdateSystem -> m (SystemTag, UpdateData)
updateDataElement ProposeUpdateSystem{..} = do
    diffHash <- hashFile pusBinDiffPath
    installerHash <- hashFile pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash installerHash dummyHash dummyHash)

-- This solution is hacky, but will work for now
runCmdOuts :: HasConfigurations => OutSpecs
runCmdOuts = relayPropagateOut $ mconcat
                [ usRelays @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , delegationRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , txRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                ]

evalCmd :: HasConfigurations => SendActions LightWalletMode -> Command -> CmdCtx -> LightWalletMode ()
evalCmd _ Quit _      = pure ()
evalCmd sa cmd cmdCtx = runCmd sa cmd cmdCtx >> evalCommands sa cmdCtx

evalCommands :: HasConfigurations => SendActions LightWalletMode -> CmdCtx -> LightWalletMode ()
evalCommands sa cmdCtx = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err   -> putStrLn err >> evalCommands sa cmdCtx
        Right cmd_ -> evalCmd sa cmd_ cmdCtx

runWalletRepl :: HasConfigurations => CmdCtx -> Worker LightWalletMode
runWalletRepl cmdCtx sa = do
    putText "Welcome to Wallet CLI Node"
    evalCmd sa Help cmdCtx

runWalletCmd :: HasConfigurations => CmdCtx -> Text -> Worker LightWalletMode
runWalletCmd cmdCtx str sa = do
    let strs = T.splitOn "," str
    for_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd sa cmd' cmdCtx
    putText "Command execution finished"
    putText " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif

main :: IO ()
main = do
    WalletOptions {..} <- getWalletOptions
    --filePeers <- maybe (return []) CLI.readPeersFile
    --                   (CLI.dhtPeersFile woCommonArgs)
    let allPeers = woPeers -- ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            }
        baseParams = BaseParams { bpLoggingParams = logParams }

    print logParams

    let params =
            WalletParams
            { wpDbPath      = Just woDbPath
            , wpRebuildDb   = woRebuildDb
            , wpKeyFilePath = woKeyFilePath
            , wpGenesisKeys = woDebug
            , wpBaseParams  = baseParams
            , ..
            }

    let cOpts = CLI.configurationOptions woCommonArgs
    loggerBracket logParams $ runProduction $ withConfigurations cOpts $
      bracketTransport TCP.Unaddressable $ \transport -> do
        logInfo $ if isDevelopment
            then "Development Mode"
            else "Production Mode"
        logInfo $ sformat ("Length of genesis utxo: "%shown)
            (length $ unGenesisUtxo genesisUtxo)
        let transport' :: Transport LightWalletMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> LightWalletMode t)
                transport

            worker' specs w = worker specs $ \sa -> w (addLogging sa)

            cmdCtx = CmdCtx
                      { skeys = fromMaybe (error "Must have secret keys") genesisSecretKeys
                      , na = woPeers
                      }

            plugins :: ([WorkerSpec LightWalletMode], OutSpecs)
            plugins = first pure $ case woAction of
                Repl    -> worker' runCmdOuts $ runWalletRepl cmdCtx
                Cmd cmd -> worker' runCmdOuts $ runWalletCmd cmdCtx cmd

        logInfo "Using MPC coin tossing"
        liftIO $ hFlush stdout
        runWalletStaticPeers transport' (S.fromList allPeers) params plugins

addLogging :: forall m. WithLogger m => SendActions m -> SendActions m
addLogging SendActions{..} = SendActions{
      enqueueMsg = error "unused"
    , withConnectionTo = aux
    }
  where
    aux nid k = withConnectionTo nid $ \peerData -> fmap auxConv (k peerData)
    auxConv (Conversation k) = Conversation (\acts -> k (auxActs acts))

    auxActs :: (Message snd, Message rcv)
            => ConversationActions snd rcv m -> ConversationActions snd rcv m
    auxActs (ConversationActions{..}) = ConversationActions {
        send = \body -> do
                 logDebug $ sformat ("Light wallet sending " % stext) (formatMessage body)
                 send body
      , recv = \limit -> do
                 mRcv <- recv limit
                 logDebug $
                   case mRcv of
                     Nothing  -> sformat ("Light wallet received end of input")
                     Just rcv -> sformat ("Light wallet received " % stext) (formatMessage rcv)
                 return mRcv
      }
