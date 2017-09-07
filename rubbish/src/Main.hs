{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Main
       ( main
       ) where

import           Universum

import qualified Data.Set                   as S (fromList)
import qualified Data.Text                  as T
#if !(defined(mingw32_HOST_OS))
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Posix.Process       (exitImmediately)
#endif
import           Formatting                 (sformat, shown, stext, (%))
import           Mockable                   (Production, delay, runProduction)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..))
import           Node.Conversation          (ConversationActions (..))
import           Node.Message.Class         (Message (..))
import           Serokell.Util              (sec)
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (logDebug, logInfo)
import           System.Wlog.CanLog         (WithLogger)

import qualified Pos.Client.CLI             as CLI
import           Pos.Communication          (Conversation (..), OutSpecs (..),
                                             SendActions (..), Worker, WorkerSpec,
                                             delegationRelays, relayPropagateOut,
                                             txRelays, usRelays, worker)
import           Pos.Constants              (isDevelopment)
import           Pos.Core.Context           (HasCoreConstants, giveStaticConsts)
import           Pos.Genesis                (devBalancesDistr, devGenesisContext,
                                             genesisContextProduction,
                                             genesisDevSecretKeys, gtcUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             bracketTransport, loggerBracket)
import           Pos.Rubbish                (LightWalletMode, WalletParams (..),
                                             runWalletStaticPeers)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Txp                    (unGenesisUtxo)
import           Pos.Util.Util              (powerLift)
import           Pos.WorkMode               (RealMode, RealModeContext)

import           Command                    (CmdCtx (..), Command (..), parseCommand,
                                             runCmd)
import           RubbishOptions             (RubbishAction (..), RubbishOptions (..),
                                             getRubbishOptions)
-- This solution is hacky, but will work for now
runCmdOuts :: HasCoreConstants => OutSpecs
runCmdOuts =
    relayPropagateOut $
    mconcat
        [ usRelays @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
        , delegationRelays
              @SscGodTossing
              @(RealModeContext SscGodTossing)
              @(RealMode SscGodTossing)
        , txRelays
              @SscGodTossing
              @(RealModeContext SscGodTossing)
              @(RealMode SscGodTossing)
        ]

evalCmd :: HasCoreConstants => SendActions LightWalletMode -> Command -> CmdCtx -> LightWalletMode ()
evalCmd _ Quit _      = pure ()
evalCmd sa cmd cmdCtx = runCmd sa cmd cmdCtx >> evalCommands sa cmdCtx

evalCommands :: HasCoreConstants => SendActions LightWalletMode -> CmdCtx -> LightWalletMode ()
evalCommands sa cmdCtx = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err   -> putStrLn err >> evalCommands sa cmdCtx
        Right cmd_ -> evalCmd sa cmd_ cmdCtx

runWalletRepl :: HasCoreConstants => CmdCtx -> Worker LightWalletMode
runWalletRepl cmdCtx sa = do
    putText "Welcome to Wallet CLI Node"
    evalCmd sa Help cmdCtx

runWalletCmd :: HasCoreConstants => CmdCtx -> Text -> Worker LightWalletMode
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
main = giveStaticConsts $ do
    RubbishOptions {..} <- getRubbishOptions
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

    let sysStart = CLI.sysStart woCommonArgs
    let devBalanceDistr =
            devBalancesDistr
                (CLI.flatDistr woCommonArgs)
                (CLI.richPoorDistr woCommonArgs)
                (CLI.expDistr woCommonArgs)
    let wpGenesisContext
            | isDevelopment = devGenesisContext devBalanceDistr
            | otherwise = genesisContextProduction
    let params =
            WalletParams
            { wpDbPath      = Just woDbPath
            , wpRebuildDb   = woRebuildDb
            , wpKeyFilePath = woKeyFilePath
            , wpSystemStart = sysStart
            , wpGenesisKeys = woDebug
            , wpBaseParams  = baseParams
            , ..
            }

    loggerBracket logParams $ runProduction $
      bracketTransport TCP.Unaddressable $ \transport -> do
        logInfo $ if isDevelopment
            then "Development Mode"
            else "Production Mode"
        logInfo $ sformat ("Length of genesis utxo: "%shown)
            (length $ unGenesisUtxo $ wpGenesisContext ^. gtcUtxo)
        let transport' :: Transport LightWalletMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> LightWalletMode t)
                transport

            worker' specs w = worker specs $ \sa -> w (addLogging sa)

            cmdCtx = CmdCtx
                      { skeys = if isDevelopment then genesisDevSecretKeys else []
                      , na = woPeers
                      , genesisBalanceDistr = devBalanceDistr
                      }

            plugins :: HasCoreConstants => ([WorkerSpec LightWalletMode], OutSpecs)
            plugins = first pure $ case woAction of
                Repl    -> worker' runCmdOuts $ runWalletRepl cmdCtx
                Cmd cmd -> worker' runCmdOuts $ runWalletCmd cmdCtx cmd

        logInfo "Using MPC coin tossing"
        liftIO $ hFlush stdout
        runWalletStaticPeers woNodeDbPath transport' (S.fromList allPeers) params plugins

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
