{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Auxx plugin.

module Plugin
       ( auxxPlugin
       , rawExec
       ) where

import           Universum

#if !(defined(mingw32_HOST_OS))
import           System.Exit (ExitCode (ExitSuccess))
import           System.Posix.Process (exitImmediately)
#endif
import           Data.Constraint (Dict (..))
import           Formatting (float, int, sformat, stext, (%))
import           Mockable (Delay, Mockable, delay)
import           Node.Conversation (ConversationActions (..))
import           Node.Message.Class (Message (..))
import           Serokell.Util (sec)
import           System.IO (hFlush, stdout)
import           System.Wlog (CanLog, HasLoggerName, WithLogger, logDebug, logInfo)

import           Pos.Communication (Conversation (..), OutSpecs (..), SendActions (..), WorkerSpec,
                                    delegationRelays, relayPropagateOut, txRelays, usRelays, worker)
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Txp (genesisUtxo, unGenesisUtxo)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog (JLEvent (JLTxReceived))
import           Pos.Util.TimeWarp (jsonLog)
import           Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext)

import           AuxxOptions (AuxxOptions (..))
import           Command (createCommandProcs)
import qualified Lang
import           Mode (MonadAuxxMode)
import           Repl (PrintAction, WithCommandAction (..))

----------------------------------------------------------------------------
-- Plugin implementation
----------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

auxxPlugin ::
       (HasCompileInfo, MonadAuxxMode m, Mockable Delay m)
    => AuxxOptions
    -> Either WithCommandAction Text
    -> (WorkerSpec m, OutSpecs)
auxxPlugin auxxOptions repl = worker' runCmdOuts $ \sendActions ->
    rawExec (Just Dict) auxxOptions (Just sendActions) repl
  where
    worker' specs w = worker specs $ \sa -> do
        logInfo $ sformat ("Length of genesis utxo: " %int)
                          (length $ unGenesisUtxo genesisUtxo)
        w (addLogging sa)

rawExec ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> AuxxOptions
    -> Maybe (SendActions m)
    -> Either WithCommandAction Text
    -> m ()
rawExec mHasAuxxMode AuxxOptions{..} mSendActions = \case
    Left WithCommandAction{..} -> do
        printAction <- getPrintAction
        printAction "... the auxx plugin is ready"
        forever $ withCommand $ runCmd mHasAuxxMode mSendActions printAction
    Right cmd -> runWalletCmd mHasAuxxMode mSendActions cmd

runWalletCmd ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> Maybe (SendActions m)
    -> Text
    -> m ()
runWalletCmd mHasAuxxMode mSendActions line = do
    runCmd mHasAuxxMode mSendActions printAction line
    printAction "Command execution finished"
    printAction " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif
  where
    printAction = putText

runCmd ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> Maybe (SendActions m)
    -> PrintAction m
    -> Text
    -> m ()
runCmd mHasAuxxMode mSendActions printAction line = do
    let commandProcs = createCommandProcs mHasAuxxMode printAction mSendActions
    case Lang.parse line of
        Left parseError -> printAction (Lang.renderAuxxDoc . Lang.ppParseError $ parseError)
        Right expr -> Lang.evaluate commandProcs expr >>= \case
            Left evalError -> printAction (Lang.renderAuxxDoc . Lang.ppEvalError $ evalError)
            Right value -> withValueText printAction value

withValueText :: Monad m => (Text -> m ()) -> Lang.Value -> m ()
withValueText cont = \case
    Lang.ValueUnit -> return ()
    Lang.ValueNumber n -> cont (sformat float n)
    Lang.ValueString s -> cont (toText s)
    Lang.ValueBool b -> cont (pretty b)
    Lang.ValueAddress a -> cont (pretty a)
    Lang.ValuePublicKey pk -> cont (sformat fullPublicKeyF pk)
    Lang.ValueTxOut txOut -> cont (pretty txOut)
    Lang.ValueStakeholderId sId -> cont (sformat hashHexF sId)
    Lang.ValueHash h -> cont (sformat hashHexF (getAHash h))
    Lang.ValueBlockVersion v -> cont (pretty v)
    Lang.ValueSoftwareVersion v -> cont (pretty v)
    Lang.ValueBlockVersionModifier bvm -> cont (pretty bvm)
    Lang.ValueBlockVersionData bvd -> cont (pretty bvd)
    Lang.ValueProposeUpdateSystem pus -> cont (show pus)
    Lang.ValueAddrDistrPart adp -> cont (show adp)
    Lang.ValueAddrStakeDistribution asd -> cont (pretty asd)
    Lang.ValueFilePath s -> cont (toText s)
    Lang.ValueSendMode sm -> cont (show sm)
    Lang.ValueList vs -> for_ vs $
        withValueText (cont . mappend "  ")



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
              logTx
        ]
  where
    logTx = jsonLog . JLTxReceived

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
      , sendRaw = sendRaw
      }
