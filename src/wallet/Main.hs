{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Reader       (MonadReader (..), ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import qualified Data.ByteString            as BS
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S (fromList, toList)
import qualified Data.Text                  as T
import           Data.Time.Units            (convertUnit)
import           Formatting                 (build, int, sformat, stext, (%))
import           Mockable                   (Production, delay)
import           Network.Transport.Abstract (Transport, hoistTransport)
import           Options.Applicative        (execParser)
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (logDebug, logError, logInfo, logWarning)
#if !(defined(mingw32_HOST_OS))
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Posix.Process       (exitImmediately)
#endif
import           Serokell.Util              (ms, sec)
import           Universum

import           Pos.Binary                 (Raw)
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (NodeId, OutSpecs, SendActions, Worker',
                                             WorkerSpec, sendTxOuts, submitTx, worker)
import           Pos.Constants              (genesisBlockVersionData, isDevelopment)
import           Pos.Crypto                 (Hash, SecretKey, SignTag (SignUSVote),
                                             createProxySecretKey, emptyPassphrase,
                                             encToPublic, fakeSigner, hash, hashHexF,
                                             noPassEncrypt, safeSign, toPublic,
                                             unsafeHash, withSafeSigner)
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.Delegation             (sendProxySKHeavy, sendProxySKHeavyOuts,
                                             sendProxySKLight, sendProxySKLightOuts)
import           Pos.Discovery              (findPeers, getPeers)
import           Pos.Genesis                (genesisDevSecretKeys,
                                             genesisStakeDistribution, genesisUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             bracketResources, stakesDistr)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Txp                    (TxOut (..), TxOutAux (..), txaF)
import           Pos.Types                  (coinF, makePubKeyAddress)
import           Pos.Update                 (BlockVersionData (..), UpdateVote (..),
                                             mkUpdateProposalWSign, patakUpdateData,
                                             skovorodaUpdateData)
import           Pos.Util.UserSecret        (readUserSecret, usKeys)
import           Pos.Util.Util              (powerLift)
import           Pos.Wallet                 (WalletMode, WalletParams (..),
                                             WalletStaticPeersMode, addSecretKey,
                                             getBalance, getSecretKeys,
                                             runWalletStaticPeers, sendProposalOuts,
                                             sendVoteOuts, submitUpdateProposal,
                                             submitVote)
#ifdef WITH_WEB
import           Pos.Wallet.Web             (walletServeWebLite, walletServerOuts)
#endif

import           Command                    (Command (..), parseCommand)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..))
import           WalletOptions              (WalletAction (..), WalletOptions (..),
                                             optsInfo)

type CmdRunner = ReaderT ([SecretKey], [NodeId])

runCmd :: WalletMode m => SendActions m -> Command -> CmdRunner m ()
runCmd _ (Balance addr) = lift (getBalance addr) >>=
                          putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) = do
    (_, na) <- ask
    skeys <- getSecretKeys
    etx <-
        lift $ withSafeSigner (skeys !! idx) (pure emptyPassphrase) $ \mss ->
        runEitherT $ do
            ss <- mss `whenNothing` throwError "Invalid passphrase"
            EitherT $
                submitTx
                    sendActions
                    ss
                    na
                    (map (flip TxOutAux []) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd sendActions (SendToAllGenesis amount delay_) = do
    (skeys, na) <- ask
    for_ skeys $ \key -> do
        let txOut = TxOut {
            txOutAddress = makePubKeyAddress (toPublic key),
            txOutValue = amount
        }
        etx <-
            lift $
            submitTx
                sendActions
                (fakeSigner key)
                na
                (NE.fromList [TxOutAux txOut []])
        case etx of
            Left err -> putText $ sformat ("Error: "%stext) err
            Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
        delay $ ms delay_
runCmd sendActions v@(Vote idx decision upid) = do
    logDebug $ "Submitting a vote :" <> show v
    (_, na) <- ask
    skeys <- getSecretKeys
    let skey = skeys !! idx
    msignature <- lift $ withSafeSigner skey (pure emptyPassphrase) $ mapM $
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
                    lift $ submitVote sendActions na voteUpd
                    putText "Submitted vote"
runCmd sendActions ProposeUpdate{..} = do
    logDebug "Proposing update..."
    (_, na) <- ask
    skeys <- getSecretKeys
    (diffFile :: Maybe (Hash Raw)) <- runMaybeT $ do
        filePath <- MaybeT $ pure puFilePath
        fileData <- liftIO $ BS.readFile filePath
        let h = unsafeHash fileData
        liftIO $ putText $ sformat ("Read file succesfuly, its hash: "%hashHexF) h
        pure h
    let skey = skeys !! puIdx
    let bvd = genesisBlockVersionData
            { bvdScriptVersion = puScriptVersion
            , bvdSlotDuration = convertUnit (sec puSlotDurationSec)
            , bvdMaxBlockSize = puMaxBlockSize
            }
    let udata = maybe patakUpdateData skovorodaUpdateData diffFile
    let whenCantCreate = error . mappend "Failed to create update proposal: "
    lift $ withSafeSigner skey (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let updateProposal = either whenCantCreate identity $
                    mkUpdateProposalWSign
                        puBlockVersion
                        bvd
                        puSoftwareVersion
                        udata
                        (mkAttributes ())
                        ss
            if null na
                then putText "Error: no addresses specified"
                else do
                    submitUpdateProposal sendActions ss na updateProposal
                    let id = hash updateProposal
                    putText $
                      sformat ("Update proposal submitted, upId: "%hashHexF) id
runCmd _ Help = do
    putText $
        unlines
            [ "Avaliable commands:"
            , "   balance <address>              -- check balance on given address (may be any address)"
            , "   send <N> [<address> <coins>]+  -- create and send transaction with given outputs"
            , "                                     from own address #N"
            , "   send-to-all-genesis <coins> <delay>  -- create and send transactions from all genesis addresses, delay in ms"
            , "                                     to themselves with the given amount of coins"
            , "   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base64) and"
            , "                                     decision, from own address #N"
            , "   propose-update <N> <block ver> <script ver> <slot duration> <max block size> <software ver> <propose_file>?"
            , "                                  -- propose an update with given versions and other data"
            , "                                     with one positive vote for it, from own address #N"
            , "   listaddr                       -- list own addresses"
            , "   delegate-light <N> <M>         -- delegate secret key #N to #M (genesis) light version"
            , "   delegate-heavy <N> <M>         -- delegate secret key #N to #M (genesis) heavyweight "
            , "   add-key-pool <N>               -- add key from intial pool"
            , "   add-key <file>                 -- add key from file"
            , "   help                           -- show this message"
            , "   quit                           -- shutdown node wallet"
            ]
runCmd _ ListAddresses = do
   addrs <- map (makePubKeyAddress . encToPublic) <$> getSecretKeys
   putText "Available addresses:"
   for_ (zip [0 :: Int ..] addrs) $
       putText . uncurry (sformat $ "    #"%int%":   "%build)
runCmd sendActions (DelegateLight i j startEpoch lastEpochM) = do
   (skeys, _) <- ask
   let issuerSk = skeys !! i
       delegatePk = toPublic $ skeys !! j
       psk = createProxySecretKey issuerSk delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
   lift $ sendProxySKLight psk sendActions
   putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i j curEpoch) = do
   (skeys, _) <- ask
   let issuerSk = skeys !! i
       delegatePk = toPublic $ skeys !! j
       psk = createProxySecretKey issuerSk delegatePk curEpoch
   lift $ sendProxySKHeavy psk sendActions
   putText "Sent heavyweight cert"
runCmd _ (AddKeyFromPool i) = do
   (skeys, _) <- ask
   let key = skeys !! i
   addSecretKey $ noPassEncrypt key
runCmd _ (AddKeyFromFile f) = do
    secret <- readUserSecret f
    mapM_ addSecretKey $ secret ^. usKeys
runCmd _ Quit = pure ()

runCmdOuts :: OutSpecs
runCmdOuts = mconcat [ sendProxySKLightOuts
                     , sendProxySKHeavyOuts
                     , sendTxOuts
                     , sendVoteOuts
                     , sendProposalOuts
                     ]

evalCmd :: WalletMode m => SendActions m -> Command -> CmdRunner m ()
evalCmd _ Quit = pure ()
evalCmd sa cmd = runCmd sa cmd >> evalCommands sa

evalCommands :: WalletMode m => SendActions m -> CmdRunner m ()
evalCommands sa = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands sa
        Right cmd -> evalCmd sa cmd

initialize :: WalletMode m => WalletOptions -> m [NodeId]
initialize WalletOptions{..} = do
    peers <- S.toList <$> getPeers
    bool (pure peers) getPeersUntilSome (null peers)
  where
    -- FIXME this is dangerous. If rmFindPeers doesn't block, for instance
    -- because it's a constant empty set of peers, we'll spin forever.
    getPeersUntilSome = do
        liftIO $ hFlush stdout
        logWarning "Discovering peers, because current peer list is empty"
        peers <- S.toList <$> findPeers
        if null peers
        then getPeersUntilSome
        else pure peers

runWalletRepl :: WalletMode m => WalletOptions -> Worker' m
runWalletRepl wo sa = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    let keysPool = if isDevelopment then genesisDevSecretKeys else []
    runReaderT (evalCmd sa Help) (keysPool, na)

runWalletCmd :: WalletMode m => WalletOptions -> Text -> Worker' m
runWalletCmd wo str sa = do
    na <- initialize wo
    let strs = T.splitOn "," str
    let keysPool = if isDevelopment then genesisDevSecretKeys else []
    flip runReaderT (keysPool, na) $ for_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd sa cmd'
    putText "Command execution finished"
    putText " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif

main :: IO ()
main = do
    opts@WalletOptions {..} <- execParser optsInfo
    --filePeers <- maybe (return []) CLI.readPeersFile
    --                   (CLI.dhtPeersFile woCommonArgs)
    let allPeers = woPeers -- ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            , lpEkgPort       = Nothing
            }
        baseParams = BaseParams { bpLoggingParams = logParams }

    bracketResources baseParams TCP.Unaddressable $ \transport -> do

        let transport' :: Transport WalletStaticPeersMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> WalletStaticPeersMode t)
                transport

        let peerId = CLI.peerId woCommonArgs
        let sysStart = CLI.sysStart woCommonArgs

        let params =
                WalletParams
                { wpDbPath      = Just woDbPath
                , wpRebuildDb   = woRebuildDb
                , wpKeyFilePath = woKeyFilePath
                , wpSystemStart = sysStart
                , wpGenesisKeys = woDebug
                , wpBaseParams  = baseParams
                , wpGenesisUtxo =
                    genesisUtxo $
                      if isDevelopment
                          then stakesDistr (CLI.flatDistr woCommonArgs)
                                           (CLI.bitcoinDistr woCommonArgs)
                                           (CLI.richPoorDistr woCommonArgs)
                                           (CLI.expDistr woCommonArgs)
                          else genesisStakeDistribution
                }

            plugins :: ([ WorkerSpec WalletStaticPeersMode ], OutSpecs)
            plugins = first pure $ case woAction of
                Repl    -> worker runCmdOuts $ runWalletRepl opts
                Cmd cmd -> worker runCmdOuts $ runWalletCmd opts cmd
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> worker walletServerOuts $ \sendActions ->
                    walletServeWebLite (Proxy @SscGodTossing)
                                       sendActions webDaedalusDbPath False webPort
#endif

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> do
                logInfo "Using MPC coin tossing"
                liftIO $ hFlush stdout
                runWalletStaticPeers peerId transport' (S.fromList allPeers) params plugins
            NistBeaconAlgo ->
                logError "Wallet does not support NIST beacon!"
