{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader      (MonadReader (..), ReaderT, ask, asks,
                                            runReaderT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString           as BS
import           Data.List                 ((!!))
import qualified Data.Text                 as T
import           Data.Time.Units           (convertUnit)
import           Formatting                (build, int, sformat, stext, (%))
import           Mockable                  (delay)
import           Options.Applicative       (execParser)
import           System.IO                 (hFlush, stdout)
import           System.Wlog               (logDebug, logError, logInfo, logWarning)
#if !(defined(mingw32_HOST_OS))
import           System.Exit               (ExitCode (ExitSuccess))
import           System.Posix.Process      (exitImmediately)
#endif
import           Serokell.Util             (sec)
import           Universum

import           Pos.Binary                (Raw)
import qualified Pos.CLI                   as CLI
import           Pos.Communication         (OutSpecs, SendActions, Worker', WorkerSpec,
                                            worker)
import           Pos.Crypto                (Hash, SecretKey, createProxySecretKey,
                                            fakeSigner, hash, hashHexF, sign, toPublic,
                                            unsafeHash)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Delegation            (sendProxySKHeavy, sendProxySKHeavyOuts,
                                            sendProxySKLight, sendProxySKLightOuts)
import           Pos.DHT.Model             (DHTNode, discoverPeers, getKnownPeers)
import           Pos.Genesis               (genesisBlockVersionData, genesisDevPublicKeys,
                                            genesisDevSecretKeys, genesisUtxo)
import           Pos.Launcher              (BaseParams (..), LoggingParams (..),
                                            bracketResources, stakesDistr)
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Ssc.NistBeacon        (SscNistBeacon)
import           Pos.Ssc.SscAlgo           (SscAlgo (..))
import           Pos.Txp                   (TxOutAux (..), txaF)
import           Pos.Types                 (EpochIndex (..), coinF, makePubKeyAddress)
import           Pos.Update                (BlockVersionData (..), UpdateProposal (..),
                                            UpdateVote (..), patakUpdateData,
                                            skovorodaUpdateData)
import           Pos.Wallet                (WalletMode, WalletParams (..), WalletRealMode,
                                            getBalance, runWalletReal, sendProposalOuts,
                                            sendTxOuts, sendVoteOuts, submitTx,
                                            submitUpdateProposal, submitVote)
#ifdef WITH_WEB
import           Pos.Wallet.Web            (walletServeWebLite, walletServerOuts)
#endif

import           Command                   (Command (..), parseCommand)
import           WalletOptions             (WalletAction (..), WalletOptions (..),
                                            optsInfo)

type CmdRunner = ReaderT ([SecretKey], [DHTNode])

runCmd :: WalletMode ssc m => SendActions m -> Command -> CmdRunner m ()
runCmd _ (Balance addr) = lift (getBalance addr) >>=
                         putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) = do
    (skeys, na) <- ask
    etx <-
        lift $
        submitTx
            sendActions
            (fakeSigner $ skeys !! idx)
            na
            (map (flip TxOutAux []) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd sendActions v@(Vote idx decision upid) = do
    logDebug $ "Submitting a vote :" <> show v
    (skeys, na) <- ask
    let skey = skeys !! idx
    let voteUpd = UpdateVote
            { uvKey        = toPublic skey
            , uvProposalId = upid
            , uvDecision   = decision
            , uvSignature  = sign skey (upid, decision)
            }
    if null na
        then putText "Error: no addresses specified"
        else do
            lift $ submitVote sendActions na voteUpd
            putText "Submitted vote"
runCmd sendActions ProposeUpdate{..} = do
    logDebug "Proposing update..."
    (skeys, na) <- ask
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
    let updateProposal = UpdateProposal
            { upBlockVersion     = puBlockVersion
            , upBlockVersionData = bvd
            , upSoftwareVersion  = puSoftwareVersion
            , upData             =
                maybe patakUpdateData
                      skovorodaUpdateData
                      diffFile
            , upAttributes       = mkAttributes ()
            }
    if null na
        then putText "Error: no addresses specified"
        else do
            lift $ submitUpdateProposal sendActions skey na updateProposal
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
            , "   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base64) and"
            , "                                     decision, from own address #N"
            , "   propose-update <N> <block ver> <script ver> <slot duration> <max block size> <software ver> <propose_file>?"
            , "                                  -- propose an update with given versions and other data"
            , "                                     with one positive vote for it, from own address #N"
            , "   listaddr                       -- list own addresses"
            , "   delegate-light <N> <M>         -- delegate secret key #N to #M (genesis) light version"
            , "   delegate-heavy <N> <M>         -- delegate secret key #N to #M (genesis) heavyweight "
            , "   help                           -- show this message"
            , "   quit                           -- shutdown node wallet"
            ]
runCmd _ ListAddresses = do
    addrs <- map (makePubKeyAddress . toPublic) <$> asks fst
    putText "Available addresses:"
    for_ (zip [0 :: Int ..] addrs) $
        putText . uncurry (sformat $ "    #"%int%":   "%build)
runCmd sendActions (DelegateLight i j) = do
    let issuerSk = genesisDevSecretKeys !! i
        delegatePk = genesisDevPublicKeys !! j
        psk = createProxySecretKey issuerSk delegatePk (EpochIndex 0, EpochIndex 50)
    lift $ sendProxySKLight psk sendActions
    putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i j epochMaybe) = do
    let issuerSk = genesisDevSecretKeys !! i
        delegatePk = genesisDevPublicKeys !! j
        epoch = fromMaybe 0 epochMaybe
        psk = createProxySecretKey issuerSk delegatePk epoch
    lift $ sendProxySKHeavy psk sendActions
    putText "Sent heavyweight cert"
runCmd _ Quit = pure ()

runCmdOuts :: OutSpecs
runCmdOuts = mconcat [ sendProxySKLightOuts
                     , sendProxySKHeavyOuts
                     , sendTxOuts
                     , sendVoteOuts
                     , sendProposalOuts
                     ]

evalCmd :: WalletMode ssc m => SendActions m -> Command -> CmdRunner m ()
evalCmd _ Quit = pure ()
evalCmd sa cmd = runCmd sa cmd >> evalCommands sa

evalCommands :: WalletMode ssc m => SendActions m -> CmdRunner m ()
evalCommands sa = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands sa
        Right cmd -> evalCmd sa cmd

initialize :: WalletMode ssc m => WalletOptions -> m [DHTNode]
initialize WalletOptions{..} = do
    peers <- getKnownPeers
    bool (pure peers) getPeersUntilSome (null peers)
  where
    getPeersUntilSome = do
        liftIO $ hFlush stdout
        logWarning "Discovering peers, because current peer list is empty"
        peers <- discoverPeers
        if null peers
        then getPeersUntilSome
        else pure peers

runWalletRepl :: WalletMode ssc m => WalletOptions -> Worker' m
runWalletRepl wo sa = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    runReaderT (evalCmd sa Help) (genesisDevSecretKeys, na)

runWalletCmd :: WalletMode ssc m => WalletOptions -> Text -> Worker' m
runWalletCmd wo str sa = do
    na <- initialize wo
    let strs = T.splitOn "," str
    flip runReaderT (genesisDevSecretKeys, na) $ for_ strs $ \scmd -> do
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
    filePeers <- maybe (return []) CLI.readPeersFile
                     (CLI.dhtPeersFile woCommonArgs)
    let allPeers = CLI.dhtPeers woCommonArgs ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            , lpEkgPort       = Nothing
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpIpPort             = woIpPort
            , bpDHTPeers           = allPeers
            , bpDHTKey             = Nothing
            , bpDHTExplicitInitial = CLI.dhtExplicitInitial woCommonArgs
            , bpKademliaDump       = "kademlia.dump"
            }
    bracketResources baseParams $ \res -> do
        -- let timeSlaveParams =
        --         baseParams
        --         { bpLoggingParams = logParams { lpRunnerTag = "time-slave" }
        --         }

        -- systemStart <- case CLI.sscAlgo woCommonArgs of
        --     GodTossingAlgo -> runTimeSlaveReal (Proxy @SscGodTossing) res timeSlaveParams
        --     NistBeaconAlgo -> runTimeSlaveReal (Proxy @SscNistBeacon) res timeSlaveParams

        let params =
                WalletParams
                { wpDbPath      = Just woDbPath
                , wpRebuildDb   = woRebuildDb
                , wpKeyFilePath = woKeyFilePath
                , wpSystemStart = error "light wallet doesn't know system start"
                , wpGenesisKeys = woDebug
                , wpBaseParams  = baseParams
                , wpGenesisUtxo =
                    genesisUtxo $
                    stakesDistr (CLI.flatDistr woCommonArgs)
                                (CLI.bitcoinDistr woCommonArgs)
                                (CLI.expDistr woCommonArgs)
                }

            plugins :: ([ WorkerSpec WalletRealMode ], OutSpecs)
            plugins = first pure $ case woAction of
                Repl                            -> worker runCmdOuts $ runWalletRepl opts
                Cmd cmd                         -> worker runCmdOuts $ runWalletCmd opts cmd
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> worker walletServerOuts $ \sendActions ->
                    case CLI.sscAlgo woCommonArgs of
                        GodTossingAlgo -> walletServeWebLite (Proxy @SscGodTossing)
                                              sendActions webDaedalusDbPath False webPort
                        NistBeaconAlgo -> walletServeWebLite (Proxy @SscNistBeacon)
                                              sendActions webDaedalusDbPath False webPort
#endif

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> do
                logInfo "Using MPC coin tossing"
                liftIO $ hFlush stdout
                runWalletReal res params plugins
            NistBeaconAlgo ->
                logError "Wallet does not support NIST beacon!"
