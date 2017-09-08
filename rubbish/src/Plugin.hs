{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Rubbish plugin.

module Plugin
       ( rubbishPlugin
       ) where

import           Universum

import qualified Data.Text            as T
#if !(defined(mingw32_HOST_OS))
import           System.Exit          (ExitCode (ExitSuccess))
import           System.Posix.Process (exitImmediately)
#endif
import           Formatting           (int, sformat, stext, (%))
import           Mockable             (delay)
import           Node.Conversation    (ConversationActions (..))
import           Node.Message.Class   (Message (..))
import           Serokell.Util        (sec)
import           System.IO            (hFlush, stdout)
import           System.Wlog          (WithLogger, logDebug, logInfo)

import           Pos.Communication    (Conversation (..), OutSpecs (..), SendActions (..),
                                       Worker, WorkerSpec, delegationRelays,
                                       relayPropagateOut, txRelays, usRelays, worker)
import           Pos.Core.Context     (HasCoreConstants)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Txp              (unGenesisUtxo)
import           Pos.Util.Util        (lensOf')
import           Pos.WorkMode         (RealMode, RealModeContext)

import           Command              (Command (..), parseCommand, runCmd)
import           Mode                 (RubbishMode)
import           RubbishOptions       (RubbishAction (..), RubbishOptions (..))

----------------------------------------------------------------------------
-- Plugin implementation
----------------------------------------------------------------------------

rubbishPlugin ::
       HasCoreConstants
    => RubbishOptions
    -> (WorkerSpec RubbishMode, OutSpecs)
rubbishPlugin RubbishOptions {..} =
    case roAction of
        Repl    -> worker' runCmdOuts $ runWalletRepl
        Cmd cmd -> worker' runCmdOuts $ runWalletCmd cmd
  where
    worker' specs w =
        worker specs $ \sa -> do
            genesisUtxo <- view lensOf'
            logInfo $
                sformat
                    ("Length of genesis utxo: " %int)
                    (length $ unGenesisUtxo genesisUtxo)
            w (addLogging sa)

evalCmd ::
       HasCoreConstants
    => SendActions RubbishMode
    -> Command
    -> RubbishMode ()
evalCmd _ Quit = pure ()
evalCmd sa cmd = runCmd sa cmd >> evalCommands sa

evalCommands ::
       HasCoreConstants => SendActions RubbishMode -> RubbishMode ()
evalCommands sa = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err   -> putStrLn err >> evalCommands sa
        Right cmd_ -> evalCmd sa cmd_

runWalletRepl :: HasCoreConstants => Worker RubbishMode
runWalletRepl sa = do
    putText "Welcome to Wallet CLI Node"
    evalCmd sa Help

runWalletCmd :: HasCoreConstants => Text -> Worker RubbishMode
runWalletCmd str sa = do
    let strs = T.splitOn "," str
    for_ strs $ \scmd -> do
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

----------------------------------------------------------------------------
-- Something hacky
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Extra logging
----------------------------------------------------------------------------

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
                 logDebug $ sformat ("Rubbish sending " % stext) (formatMessage body)
                 send body
      , recv = \limit -> do
                 mRcv <- recv limit
                 logDebug $
                   case mRcv of
                     Nothing  -> sformat ("Rubbish received end of input")
                     Just rcv -> sformat ("Rubbish received " % stext) (formatMessage rcv)
                 return mRcv
      }
