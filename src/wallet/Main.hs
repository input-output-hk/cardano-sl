{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Monad.Reader   (MonadReader (..), ReaderT, asks, runReaderT)
import           Control.TimeWarp.Rpc   (NetworkAddress)
import           Control.TimeWarp.Timed (for, wait)
import           Data.List              ((!!))
import           Formatting             (build, int, sformat, (%))
import           Options.Applicative    (execParser)
import           System.IO              (hFlush, stdout)
import           Universum

import           Pos.Communication      (sendProxySecretKey)
import           Pos.Constants          (slotDuration)
import           Pos.Crypto             (SecretKey, createProxySecretKey, toPublic)
import           Pos.DHT.Model          (DHTNodeType (..), dhtAddr, discoverPeers)
import           Pos.Genesis            (genesisPublicKeys, genesisSecretKeys)
import           Pos.Launcher           (BaseParams (..), LoggingParams (..),
                                         bracketDHTInstance, runTimeSlaveReal)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))
import           Pos.Types              (EpochIndex (..), makePubKeyAddress, txwF)
import           Pos.Wallet             (WalletMode, WalletParams (..), WalletRealMode,
                                         getBalance, runWalletReal, submitTx)
#ifdef WITH_WEB
import           Pos.Wallet.Web         (walletServeWeb)
#endif

import           Command                (Command (..), parseCommand)
import           WalletOptions          (WalletAction (..), WalletOptions (..), optsInfo)

type CmdRunner = ReaderT ([SecretKey], [NetworkAddress])

evalCmd :: WalletMode ssc m => Command -> CmdRunner m ()
evalCmd (Balance addr) = lift (getBalance addr) >>=
                         putText . sformat ("Current balance: "%int) >>
                         evalCommands
evalCmd (Send idx outputs) = do
    (skeys, na) <- ask
    tx <- lift $ submitTx (skeys !! idx) na outputs
    putText $ sformat ("Submitted transaction: "%txwF) tx
    evalCommands
evalCmd Help = do
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
    evalCommands
evalCmd ListAddresses = do
    addrs <- map (makePubKeyAddress . toPublic) <$> asks fst
    putText "Available addrsses:"
    forM_ (zip [0 :: Int ..] addrs) $
        putText . uncurry (sformat $ "    #"%int%":   "%build)
    evalCommands
evalCmd (Delegate i j) = do
    let issuerSk = genesisSecretKeys !! i
        delegatePk = genesisPublicKeys !! j
        proxySig =
            createProxySecretKey issuerSk delegatePk (EpochIndex 0, EpochIndex 50)
    putText $ pretty issuerSk
    putText $ pretty delegatePk
    putText "sending cert"
    sendProxySecretKey proxySig
    putText "sent cert"
    evalCommands
evalCmd Quit = pure ()

evalCommands :: WalletMode ssc m => CmdRunner m ()
evalCommands = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands
        Right cmd -> evalCmd cmd

runWalletRepl :: WalletMode ssc m => WalletOptions -> m ()
runWalletRepl WalletOptions{..} = do
    -- Wait some time to ensure blockchain is fetched
    putText $ sformat ("Started node. Waiting for "%int%" slots...") woInitialPause
    wait $ for $ fromIntegral woInitialPause * slotDuration

    na <- fmap dhtAddr <$> discoverPeers DHTFull
    putText "Welcome to Wallet CLI Node"
    runReaderT (evalCmd Help) (genesisSecretKeys, na)

main :: IO ()
main = do
    opts@WalletOptions {..} <- execParser optsInfo
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = woLogsPrefix
            , lpConfigPath    = woLogConfig
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpPort               = woPort
            , bpDHTPeers           = woDHTPeers
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = woDhtExplicitInitial
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
#ifdef WITH_WEB
                Serve webPort webDaedalusDbPath -> [walletServeWeb webDaedalusDbPath webPort]
#endif

        case woSscAlgo of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runWalletReal inst params plugins
            NistBeaconAlgo -> putText "Wallet does not support NIST beacon!"
