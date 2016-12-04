{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Monad.Reader   (MonadReader (..), ReaderT, runReaderT)
import           Control.TimeWarp.Rpc   (NetworkAddress)
import           Control.TimeWarp.Timed (for, fork_, wait)
import           Data.List              ((!!))
import           Formatting             (int, sformat, (%))
import           Options.Applicative    (execParser)
import           System.IO              (hFlush, stdout)
import           Test.QuickCheck        (arbitrary, generate)
import           Universum

import           Pos.Constants          (slotDuration)
import           Pos.Crypto             (KeyPair (..), SecretKey)
import           Pos.DHT                (DHTNodeType (..), dhtAddr, discoverPeers)
import           Pos.DHT.Real           (KademliaDHTInstance)
import           Pos.Genesis            (genesisSecretKeys, genesisUtxo)
import           Pos.Launcher           (BaseParams (..), LoggingParams (..),
                                         NodeParams (..), bracketDHTInstance, runNode,
                                         runProductionMode, runTimeSlaveReal, stakesDistr)
import           Pos.Ssc.Class          (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing     (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon     (SscNistBeacon)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))
import           Pos.Types              (txF)
import           Pos.Wallet             (getBalance, submitTx)
import           Pos.WorkMode           (WorkMode)

import           Command                (Command (..), parseCommand)
import           WalletOptions          (WalletOptions (..), optsInfo)

type CmdRunner = ReaderT (SecretKey, [NetworkAddress])

evalCmd :: WorkMode ssc m => Command -> CmdRunner m ()
evalCmd Quit = pure ()
evalCmd (Balance addr) = lift (getBalance addr) >>=
                         putText . sformat ("Current balance: "%int) >>
                         evalCommands
evalCmd (Send outputs) = do
    (sk, na) <- ask
    tx <- lift (submitTx sk na outputs)
    putText $ sformat ("Submitted transaction: "%txF) tx
    evalCommands

evalCommands :: WorkMode ssc m => CmdRunner m ()
evalCommands = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err  -> putStrLn err >> evalCommands
        Right cmd -> evalCmd cmd

runWallet :: forall ssc . SscConstraint ssc
          => KademliaDHTInstance -> NodeParams -> SscParams ssc -> WalletOptions -> IO ()
runWallet inst np@NodeParams{..} sscnp WalletOptions{..} =
    runProductionMode inst np sscnp $ do

    -- Run node workers
    fork_ $ runNode @ssc []

    -- Wait some time to ensure blockchain is fetched
    putText $ sformat ("Started node. Waiting for "%int%" slots...") woInitialPause
    wait $ for $ fromIntegral woInitialPause * slotDuration

    let sk = genesisSecretKeys !! woSecretKeyIdx
    na <- fmap dhtAddr <$> discoverPeers DHTFull
    runReaderT evalCommands (sk, na)

main :: IO ()
main = do
    opts@WalletOptions {..} <- execParser optsInfo

    KeyPair _ sk <- generate arbitrary
    vssKeyPair <- generate arbitrary
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = woLogsPrefix
            , lpConfigPath    = woLogConfig
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpPort               = 24961
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
                NodeParams
                { npDbPath      = Nothing
                , npRebuildDb   = False
                , npSystemStart = systemStart
                , npSecretKey   = sk
                , npBaseParams  = baseParams
                , npCustomUtxo  = Just $ genesisUtxo $
                                  stakesDistr woFlatDistr woBitcoinDistr
                , npTimeLord    = False
                , npJLFile      = woJLFile
                }
            gtParams =
                GtParams
                { gtpRebuildDb  = False
                , gtpDbPath     = Nothing
                , gtpSscEnabled = False
                , gtpVssKeyPair = vssKeyPair
                }

        case woSscAlgo of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runWallet @SscGodTossing inst params gtParams opts
            NistBeaconAlgo -> putText "Using NIST beacon" *>
                              runWallet @SscNistBeacon inst params () opts

