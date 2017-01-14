{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader (MonadReader (..), ReaderT, ask, asks, runReaderT)
import           Data.List            ((!!))
import           Data.Proxy           (Proxy (..))
import qualified Data.Text            as T
import           Formatting           (build, int, sformat, stext, (%))
import           Mockable             (delay)
import           Node                 (SendActions, hoistSendActions)
import           Options.Applicative  (execParser)
import           System.IO            (hFlush, stdout)
import           Universum

import qualified Pos.CLI              as CLI
import           Pos.Communication    (BiP)
import           Pos.Constants        (slotDuration)
import           Pos.Crypto           (SecretKey, createProxySecretKey, toPublic)
import           Pos.Delegation       (sendProxySKEpoch, sendProxySKSimple)
import           Pos.DHT.Model        (DHTNodeType (..), dhtAddr, discoverPeers)
import           Pos.Genesis          (genesisPublicKeys, genesisSecretKeys)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       bracketResources, runTimeSlaveReal)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Ssc.NistBeacon   (SscNistBeacon)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Types            (EpochIndex (..), coinF, makePubKeyAddress, txaF)
import           Pos.Util.TimeWarp    (NetworkAddress)
import           Pos.Wallet           (WalletMode, WalletParams (..), WalletRealMode,
                                       getBalance, runWalletReal, submitTx)
#ifdef WITH_WEB
import           Pos.Wallet.Web       (walletServeWebLite)
#endif

import           Command              (Command (..), parseCommand)
import           WalletOptions        (WalletAction (..), WalletOptions (..), optsInfo)

type CmdRunner = ReaderT ([SecretKey], [NetworkAddress])

runCmd :: WalletMode ssc m => SendActions BiP m -> Command -> CmdRunner m ()
runCmd _ (Balance addr) = lift (getBalance addr) >>=
                         putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) = do
    (skeys, na) <- ask
    etx <- lift $ submitTx sendActions (skeys !! idx) na (map (,[]) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd _ Help = do
    putText $
        unlines
            [ "Avaliable commands:"
            , "   balance <address>              -- check balance on given address (may be any address)"
            , "   send <N> [<address> <coins>]+  -- create and send transaction with given outputs"
            , "                                     from own address #N"
            , "   listaddr                       -- list own addresses"
            , "   delegate-light <N> <M>         -- delegate secret key #N to #M (genesis) light version"
            , "   delegate-heavy <N> <M>         -- delegate secret key #N to #M (genesis) heavyweight "
            , "   help                           -- show this message"
            , "   quit                           -- shutdown node wallet"
            ]
runCmd _ ListAddresses = do
    addrs <- map (makePubKeyAddress . toPublic) <$> asks fst
    putText "Available addrsses:"
    forM_ (zip [0 :: Int ..] addrs) $
        putText . uncurry (sformat $ "    #"%int%":   "%build)
runCmd sendActions (DelegateLight i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
    r <- ask
    sendProxySKEpoch (hoistSendActions lift (`runReaderT` r) sendActions) $
        createProxySecretKey issuerSk delegatePk (EpochIndex 0, EpochIndex 50)
    putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
    r <- ask
    sendProxySKSimple (hoistSendActions lift (`runReaderT` r) sendActions) $
        createProxySecretKey issuerSk delegatePk ()
    putText "Sent heavyweight cert"
runCmd _ Quit = pure ()

evalCmd :: WalletMode ssc m => SendActions BiP m -> Command -> CmdRunner m ()
evalCmd _ Quit = pure ()
evalCmd sa cmd = runCmd sa cmd >> evalCommands sa

evalCommands :: WalletMode ssc m => SendActions BiP m -> CmdRunner m ()
evalCommands sa = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands sa
        Right cmd -> evalCmd sa cmd

initialize :: WalletMode ssc m => WalletOptions -> m [NetworkAddress]
initialize WalletOptions{..} = do
    -- Wait some time to ensure blockchain is fetched
    putText $ sformat ("Started node. Waiting for "%int%" slots...") woInitialPause
    delay $ fromIntegral woInitialPause * slotDuration
    fmap dhtAddr <$> discoverPeers DHTFull

runWalletRepl :: WalletMode ssc m => WalletOptions -> SendActions BiP m -> m ()
runWalletRepl wo sa = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    runReaderT (evalCmd sa Help) (genesisSecretKeys, na)

runWalletCmd :: WalletMode ssc m => WalletOptions -> Text -> SendActions BiP m -> m ()
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

main :: IO ()
main = do
    opts@WalletOptions {..} <- execParser optsInfo
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpIpPort             = woIpPort
            , bpDHTPeers           = CLI.dhtPeers woCommonArgs
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = CLI.dhtExplicitInitial woCommonArgs
            , bpNtpPort            = fromMaybe (snd woIpPort + 1000) woNtpPort
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

            plugins :: [SendActions BiP WalletRealMode -> WalletRealMode ()]
            plugins = case woAction of
                Repl          -> [runWalletRepl opts]
                Cmd cmd       -> [runWalletCmd opts cmd]
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> [\sendActions ->
                    walletServeWebLite sendActions webDaedalusDbPath False webPort]
#endif

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runWalletReal res params plugins
            NistBeaconAlgo -> putText "Wallet does not support NIST beacon!"
