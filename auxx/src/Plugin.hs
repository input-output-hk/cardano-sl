{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Auxx plugin.

module Plugin
       ( auxxPlugin
       ) where

import           Universum

import qualified Data.Text                  as T
#if !(defined(mingw32_HOST_OS))
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Posix.Process       (exitImmediately)
#endif
import           Formatting                 (int, sformat, stext, (%))
import           Mockable                   (delay)
import           Node.Conversation          (ConversationActions (..))
import           Node.Message.Class         (Message (..))
import           Serokell.Util              (sec)
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (WithLogger, logDebug, logInfo)

import           Pos.Communication          (Conversation (..), OutSpecs (..),
                                             SendActions (..), Worker, WorkerSpec,
                                             delegationRelays, relayPropagateOut,
                                             txRelays, usRelays, worker)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Txp                    (genesisUtxo, unGenesisUtxo)
import           Pos.Util.CompileInfo       (HasCompileInfo)
import           Pos.WorkMode               (EmptyMempoolExt, RealMode, RealModeContext)

import           AuxxOptions                (AuxxOptions (..))
import           Command                    (parseCommand, runCmd)
import           Mode                       (AuxxMode)
import           Repl                       (WithCommandAction (..))

----------------------------------------------------------------------------
-- Plugin implementation
----------------------------------------------------------------------------

auxxPlugin ::
       (HasConfigurations, HasCompileInfo)
    => AuxxOptions
    -> Either (WithCommandAction AuxxMode) Text
    -> (WorkerSpec AuxxMode, OutSpecs)
auxxPlugin AuxxOptions{..} = \case
        Left WithCommandAction{..} -> worker' runCmdOuts $ \sendActions -> do
            printAction <- getPrintAction
            printAction "... the auxx plugin is ready"
            forever $ withCommand $ \cmd -> do
                runCmd cmd printAction sendActions
        Right cmd -> worker' runCmdOuts $ runWalletCmd cmd
  where
    worker' specs w =
        worker specs $ \sa -> do
            logInfo $ sformat ("Length of genesis utxo: " %int)
                              (length $ unGenesisUtxo genesisUtxo)
            w (addLogging sa)

runWalletCmd :: (HasConfigurations, HasCompileInfo) => Text -> Worker AuxxMode
runWalletCmd str sa = do
    let strs = T.splitOn "," str
    for_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd cmd' putText sa
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
runCmdOuts :: (HasConfigurations,HasCompileInfo) => OutSpecs
runCmdOuts =
    relayPropagateOut $
    mconcat
        [ usRelays
              @(RealModeContext EmptyMempoolExt)
              @(RealMode EmptyMempoolExt)
        , delegationRelays
              @(RealModeContext EmptyMempoolExt)
              @(RealMode EmptyMempoolExt)
        , txRelays
              @(RealModeContext EmptyMempoolExt)
              @(RealMode EmptyMempoolExt)
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
                 logDebug $ sformat ("Auxx sending " % stext) (formatMessage body)
                 send body
      , recv = \limit -> do
                 mRcv <- recv limit
                 logDebug $
                   case mRcv of
                     Nothing  -> sformat ("Auxx received end of input")
                     Just rcv -> sformat ("Auxx received " % stext) (formatMessage rcv)
                 return mRcv
      }
