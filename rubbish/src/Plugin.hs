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
import           Formatting           (sformat, stext, (%))
import           Mockable             (delay)
import           Node.Conversation    (ConversationActions (..))
import           Node.Message.Class   (Message (..))
import           Serokell.Util        (sec)
import           System.IO            (hFlush, stdout)
import           System.Wlog          (logDebug)
import           System.Wlog.CanLog   (WithLogger)

import           Pos.Communication    (Conversation (..), OutSpecs (..), SendActions (..),
                                       Worker, WorkerSpec, delegationRelays,
                                       relayPropagateOut, txRelays, usRelays, worker)
import           Pos.Core.Context     (HasCoreConstants)
import           Pos.Rubbish          (LightWalletMode)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.WorkMode         (RealMode, RealModeContext)

import           Command              (CmdCtx (..), Command (..), parseCommand, runCmd)
import           RubbishOptions       (RubbishAction (..), RubbishOptions (..))

----------------------------------------------------------------------------
-- Plugin implementation
----------------------------------------------------------------------------

rubbishPlugin ::
       HasCoreConstants
    => CmdCtx
    -> RubbishOptions
    -> (WorkerSpec LightWalletMode, OutSpecs)
rubbishPlugin cmdCtx RubbishOptions {..} =
    case woAction of
        Repl    -> worker' runCmdOuts $ runWalletRepl cmdCtx
        Cmd cmd -> worker' runCmdOuts $ runWalletCmd cmdCtx cmd
  where
    worker' specs w = worker specs $ \sa -> w (addLogging sa)

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
