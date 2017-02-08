{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader      (MonadReader (..), ReaderT, ask, asks,
                                            runReaderT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString           as BS
import           Data.List                 ((!!))
import           Data.Proxy                (Proxy (..))
import qualified Data.Text                 as T
import           Data.Time.Units           (convertUnit)
import           Formatting                (build, int, sformat, stext, (%))
import           Mockable                  (delay)
import           Options.Applicative       (execParser)
import           System.IO                 (hFlush, stdout)
import           Universum
#if !(defined(mingw32_HOST_OS) && defined(__MINGW32__))
import           System.Exit               (ExitCode (ExitSuccess))
import           System.Posix.Process      (exitImmediately)
#endif

import qualified Pos.CLI                   as CLI
import           Pos.Communication         (OutSpecs, SendActions, Worker', WorkerSpec,
                                            worker)
import           Pos.Crypto                (Hash, SecretKey, createProxySecretKey,
                                            encodeHash, hash, hashHexF, sign, toPublic,
                                            unsafeHash)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Delegation            (sendProxySKEpoch, sendProxySKEpochOuts,
                                            sendProxySKSimple, sendProxySKSimpleOuts)
import           Pos.DHT.Model             (DHTNode, discoverPeers)
import           Pos.Genesis               (genesisBlockVersionData, genesisPublicKeys,
                                            genesisSecretKeys)
import           Pos.Launcher              (BaseParams (..), LoggingParams (..),
                                            bracketResources, runTimeSlaveReal)
import           Pos.Slotting              (getSlotDuration)
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Ssc.NistBeacon        (SscNistBeacon)
import           Pos.Ssc.SscAlgo           (SscAlgo (..))
import           Pos.Types                 (EpochIndex (..), coinF, makePubKeyAddress,
                                            txaF)
import           Pos.Update                (BlockVersionData (..), UpdateProposal (..),
                                            UpdateVote (..), patakUpdateData,
                                            skovorodaUpdateData)
import           Pos.Util                  (Raw)
import           Pos.Util.TimeWarp         (sec)
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
    etx <- lift $ submitTx sendActions (skeys !! idx) na (map (,[]) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd sendActions (Vote idx decision upid) = do
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
    putText "Proposing update..."
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
              sformat ("Update proposal submitted, upId: "%build%" (base64)") (encodeHash id)
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
            , "   listaddr                       -- list own addresses"
            , "   delegate-light <N> <M>         -- delegate secret key #N to #M (genesis) light version"
            , "   delegate-heavy <N> <M>         -- delegate secret key #N to #M (genesis) heavyweight "
            , "   help                           -- show this message"
            , "   quit                           -- shutdown node wallet"
            ]
runCmd _ ListAddresses = do
    addrs <- map (makePubKeyAddress . toPublic) <$> asks fst
    putText "Available addresses:"
    forM_ (zip [0 :: Int ..] addrs) $
        putText . uncurry (sformat $ "    #"%int%":   "%build)
runCmd sendActions (DelegateLight i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
        psk = createProxySecretKey issuerSk delegatePk (EpochIndex 0, EpochIndex 50)
    lift $ sendProxySKEpoch psk sendActions
    putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
        psk = createProxySecretKey issuerSk delegatePk ()
    lift $ sendProxySKSimple psk sendActions
    putText "Sent heavyweight cert"
runCmd _ Quit = pure ()

runCmdOuts :: OutSpecs
runCmdOuts = mconcat [ sendProxySKEpochOuts
                     , sendProxySKSimpleOuts
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
    -- Wait some time to ensure blockchain is fetched
    unless (woInitialPause == 0) $ do
        putText $ sformat ("Started node. Waiting for "%int%" slots...") woInitialPause
        slotDuration <- getSlotDuration
        delay (fromIntegral woInitialPause * slotDuration)
    putText "Discovering peers"
    let getPeersUntilSome = do
            peers <- discoverPeers
            if null peers then getPeersUntilSome else pure peers
    getPeersUntilSome

runWalletRepl :: WalletMode ssc m => WalletOptions -> Worker' m
runWalletRepl wo sa = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    runReaderT (evalCmd sa Help) (genesisSecretKeys, na)

runWalletCmd :: WalletMode ssc m => WalletOptions -> Text -> Worker' m
runWalletCmd wo str sa = do
    na <- initialize wo
    let strs = T.splitOn "," str
    flip runReaderT (genesisSecretKeys, na) $ forM_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd sa cmd'
    putText "Command execution finished"
    putText " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
    liftIO $ hFlush stderr
#if !(defined(mingw32_HOST_OS) && defined(__MINGW32__))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif

main :: IO ()
main = do
    opts@WalletOptions {..} <- execParser optsInfo
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
            , bpDHTPeers           = CLI.dhtPeers woCommonArgs
            , bpDHTKey             = Nothing
            , bpDHTExplicitInitial = CLI.dhtExplicitInitial woCommonArgs
            , bpKademliaDump       = "kademlia.dump"
            }
    bracketResources baseParams $ \res -> do
        let timeSlaveParams =
                baseParams
                { bpLoggingParams = logParams { lpRunnerTag = "time-slave" }
                }

        systemStart <- case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> runTimeSlaveReal (Proxy :: Proxy SscGodTossing) res timeSlaveParams
            NistBeaconAlgo -> runTimeSlaveReal (Proxy :: Proxy SscNistBeacon) res timeSlaveParams

        let params =
                WalletParams
                { wpDbPath      = Just woDbPath
                , wpRebuildDb   = woRebuildDb
                , wpKeyFilePath = woKeyFilePath
                , wpSystemStart = systemStart
                , wpGenesisKeys = woDebug
                , wpBaseParams  = baseParams
                }

            plugins :: ([ WorkerSpec WalletRealMode ], OutSpecs)
            plugins = first pure $ case woAction of
                Repl          -> worker runCmdOuts $ runWalletRepl opts
                Cmd cmd       -> worker runCmdOuts $ runWalletCmd opts cmd
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> worker walletServerOuts $ \sendActions ->
                    walletServeWebLite sendActions webDaedalusDbPath False webPort
#endif

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runWalletReal res params plugins
            NistBeaconAlgo -> putText "Wallet does not support NIST beacon!"
