{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import           Control.TimeWarp.Rpc (NetworkAddress)
import           Data.List            ((!!))
import qualified Data.Text            as T
import           Formatting           (build, int, sformat, stext, (%))
import           Mockable             (delay, for)
import           Options.Applicative  (execParser)
import           System.IO            (hFlush, stdout)
import           Universum

import qualified Pos.CLI              as CLI
import           Pos.Communication    (sendProxySecretKey)
import           Pos.Constants        (slotDuration)
import           Pos.Crypto           (SecretKey, createProxySecretKey, toPublic)
import           Pos.Genesis          (genesisPublicKeys, genesisSecretKeys)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       bracketDHTInstance, runTimeSlaveReal)
import           Pos.NewDHT.Model     (DHTNodeType (..), dhtAddr, discoverPeers)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Types            (EpochIndex (..), coinF, makePubKeyAddress, txaF)
import           Pos.Wallet           (WalletMode, WalletParams (..), WalletRealMode,
                                       getBalance, runWalletReal, submitTx)
#ifdef WITH_WEB
import           Pos.Wallet.Web       (walletServeWebLite)
#endif

import           Command              (Command (..), parseCommand)
import           WalletOptions        (WalletAction (..), WalletOptions (..), optsInfo)

type CmdRunner = ReaderT ([SecretKey], [NetworkAddress])

runCmd :: WalletMode ssc m => Command -> CmdRunner m ()
runCmd (Balance addr) = lift (getBalance addr) >>=
                         putText . sformat ("Current balance: "%coinF)
runCmd (Send idx outputs) = do
    (skeys, na) <- ask
    etx <- lift $ submitTx (skeys !! idx) na (map (,[]) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd Help = do
    putText $
        unlines
            [ "Avaliable commands:"
            , "   balance <address>              -- check balance on given address (may be any address)"
            , "   send <N> [<address> <coins>]+  -- create and send transaction with given outputs"
            , "                                     from own address #N"
            , "   listaddr                       -- list own addresses"
            , "   delegate <N> <M>               -- delegate secret key #N to #M (genesis)"
            , "   help                           -- show this message"
            , "   quit                           -- shutdown node wallet"
            ]
runCmd ListAddresses = do
    addrs <- map (makePubKeyAddress . toPublic) <$> asks fst
    putText "Available addrsses:"
    forM_ (zip [0 :: Int ..] addrs) $
        putText . uncurry (sformat $ "    #"%int%":   "%build)
runCmd (Delegate i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
        proxySig =
            createProxySecretKey issuerSk delegatePk (EpochIndex 0, EpochIndex 50)
    putText $ pretty issuerSk
    putText $ pretty delegatePk
    putText "sending cert"
    -- TODO [CSL-447] Uncomment
    --sendProxySecretKey proxySig
    putText "sent cert"
runCmd Quit = pure ()

evalCmd :: WalletMode ssc m => Command -> CmdRunner m ()
evalCmd Quit = pure ()
evalCmd cmd  = runCmd cmd >> evalCommands

evalCommands :: WalletMode ssc m => CmdRunner m ()
evalCommands = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands
        Right cmd -> evalCmd cmd

initialize :: WalletMode ssc m => WalletOptions -> m [NetworkAddress]
initialize WalletOptions{..} = do
    -- Wait some time to ensure blockchain is fetched
    putText $ sformat ("Started node. Waiting for "%int%" slots...") woInitialPause
    delay $ for $ fromIntegral woInitialPause * slotDuration
    fmap dhtAddr <$> discoverPeers DHTFull

runWalletRepl :: WalletMode ssc m => WalletOptions -> m ()
runWalletRepl wo = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    runReaderT (evalCmd Help) (genesisSecretKeys, na)

runWalletCmd :: WalletMode ssc m => WalletOptions -> Text -> m ()
runWalletCmd wo str = do
    na <- initialize wo
    let strs = T.splitOn "," str
    flip runReaderT (genesisSecretKeys, na) $ forM_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd cmd'
    liftIO exitSuccess

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
            , bpPort               = woPort
            , bpDHTPeers           = CLI.dhtPeers woCommonArgs
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = CLI.dhtExplicitInitial woCommonArgs
            }

    bracketDHTInstance baseParams $ \inst -> do
        let timeSlaveParams =
                baseParams
                { bpLoggingParams = logParams { lpRunnerTag = "time-slave" }
                }

        systemStart <- runTimeSlaveReal inst timeSlaveParams

        let params =
                WalletParams
                { wpDbPath      = Just woDbPath
                , wpRebuildDb   = woRebuildDb
                , wpKeyFilePath = woKeyFilePath
                , wpSystemStart = systemStart
                , wpGenesisKeys = woDebug
                , wpBaseParams  = baseParams
                }

            plugins :: [WalletRealMode ()]
            plugins = case woAction of
                Repl          -> [runWalletRepl opts]
                Cmd cmd       -> [runWalletCmd opts cmd]
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> [walletServeWebLite webDaedalusDbPath False webPort]
#endif

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runWalletReal inst params plugins
            NistBeaconAlgo -> putText "Wallet does not support NIST beacon!"
