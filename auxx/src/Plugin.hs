{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Control.Monad.Except (ExceptT (..), withExceptT)
import           Data.Constraint (Dict (..))
import           Data.Time.Units (Second)
import           Formatting (float, int, sformat, (%))
import           System.IO (hFlush, stdout)
import           System.Wlog (CanLog, HasLoggerName, logInfo)

import           Pos.Chain.Txp (TxpConfiguration, genesisUtxo, unGenesisUtxo)
import           Pos.Core.Conc (delay)
import           Pos.Crypto (AHash (..), ProtocolMagic, fullPublicKeyF,
                     hashHexF)
import           Pos.Infra.Diffusion.Types (Diffusion)

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
       MonadAuxxMode m
    => ProtocolMagic
    -> TxpConfiguration
    -> AuxxOptions
    -> Either WithCommandAction Text
    -> Diffusion m
    -> m ()
auxxPlugin pm txpConfig auxxOptions repl = \diffusion -> do
    logInfo $ sformat ("Length of genesis utxo: " %int)
                      (length $ unGenesisUtxo genesisUtxo)
    rawExec (Just pm) (Just txpConfig) (Just Dict) auxxOptions (Just diffusion) repl

rawExec ::
       ( MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       )
    => Maybe ProtocolMagic
    -> Maybe TxpConfiguration
    -> Maybe (Dict (MonadAuxxMode m))
    -> AuxxOptions
    -> Maybe (Diffusion m)
    -> Either WithCommandAction Text
    -> m ()
rawExec pm txpConfig mHasAuxxMode AuxxOptions{..} mDiffusion = \case
    Left WithCommandAction{..} -> do
        printAction "... the auxx plugin is ready"
        forever $ withCommand $ runCmd pm txpConfig mHasAuxxMode mDiffusion printAction
    Right cmd -> runWalletCmd pm txpConfig mHasAuxxMode mDiffusion cmd

runWalletCmd ::
       ( MonadIO m
       , CanLog m
       , HasLoggerName m
       )
    => Maybe ProtocolMagic
    -> Maybe TxpConfiguration
    -> Maybe (Dict (MonadAuxxMode m))
    -> Maybe (Diffusion m)
    -> Text
    -> m ()
runWalletCmd pm txpConfig mHasAuxxMode mDiffusion line = do
    runCmd pm txpConfig mHasAuxxMode mDiffusion printAction line
    printAction "Command execution finished"
    printAction " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay (3 :: Second)
    liftIO $ exitImmediately ExitSuccess
#endif
  where
    printAction = putText

runCmd ::
       ( MonadIO m
       , CanLog m
       , HasLoggerName m
       )
    => Maybe ProtocolMagic
    -> Maybe TxpConfiguration
    -> Maybe (Dict (MonadAuxxMode m))
    -> Maybe (Diffusion m)
    -> PrintAction m
    -> Text
    -> m ()
runCmd pm txpConfig mHasAuxxMode mDiffusion printAction line = do
    let commandProcs = createCommandProcs pm txpConfig mHasAuxxMode printAction mDiffusion
        parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
        resolveCommandProcs =
            withExceptT Lang.ppResolveErrors . ExceptT . return .
            Lang.resolveCommandProcs commandProcs
        evaluate = withExceptT Lang.ppEvalError . ExceptT . Lang.evaluate
        pipeline = parse >=> resolveCommandProcs >=> evaluate
    runExceptT (pipeline line) >>= \case
        Left errDoc -> printAction (Lang.renderAuxxDoc errDoc)
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
    Lang.ValueList vs -> for_ vs $
        withValueText (cont . mappend "  ")

----------------------------------------------------------------------------
-- Extra logging
----------------------------------------------------------------------------

-- This addLogging was misplaced to begin with.
-- A debug-mode diffusion layer could be chosen, which logs absolutely all
-- network activity. But surely for auxx logging, the logging should go around
-- the high-level auxx commands, no?
{-
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
-}
