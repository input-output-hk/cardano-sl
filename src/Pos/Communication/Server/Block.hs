{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockListeners

       , handleBlock
       , handleBlockHeader
       , handleBlockRequest
       ) where

import           Control.TimeWarp.Logging   (logDebug, logInfo)
import           Formatting                 (build, sformat, stext, (%))
import           Pos.DHT                    (ListenerDHT (..), replyToNode)
import           Serokell.Util              (VerificationRes (..), listBuilderJSON)
import           Universum

import           Control.TimeWarp.Rpc       (BinaryP, MonadDialog)
import           Pos.Communication.Types    (RequestBlock (..), ResponseMode,
                                             SendBlock (..), SendBlockHeader (..))
import           Pos.Communication.Util     (modifyListenerLogger)
import           Pos.Crypto                 (hash)
import           Pos.DHT                    (ListenerDHT (..), replyToNode)
import           Pos.Slotting               (getCurrentSlot)
import           Pos.Ssc.DynamicState.Types (SscDynamicState)
import qualified Pos.State                  as St
import           Pos.Statistics             (statlogReceivedBlock,
                                             statlogReceivedBlockHeader, statlogSentBlock)
import           Pos.WorkMode               (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
blockListeners =
    map (modifyListenerLogger "block")
        [ ListenerDHT handleBlock
        , ListenerDHT handleBlockHeader
        , ListenerDHT handleBlockRequest
        ]

handleBlock :: ResponseMode m => SendBlock SscDynamicState -> m ()
handleBlock (SendBlock block) = do
    statlogReceivedBlock block
    slotId <- getCurrentSlot
    pbr <- St.processBlock slotId block
    case pbr of
        St.PBRabort msg -> do
            let fmt =
                    "Block processing is aborted for the following reason: "%stext
            logInfo $ sformat fmt msg
        St.PBRgood _ -> logInfo $ "Received block has been adopted"
        St.PBRmore h -> replyToNode $ RequestBlock h

handleBlockHeader
    :: ResponseMode m
    => SendBlockHeader SscDynamicState -> m ()
handleBlockHeader (SendBlockHeader header) = do
    statlogReceivedBlockHeader header'
    whenM checkUsefulness $ replyToNode (RequestBlock h)
  where
    header' = Right header
    h = hash header'
    checkUsefulness = do
        slotId <- getCurrentSlot
        verRes <- St.mayBlockBeUseful slotId header
        case verRes of
            VerFailure errors -> do
                let fmt =
                        "Ignoring header with hash "%build%
                        " for the following reasons: "%build
                let msg = sformat fmt h (listBuilderJSON errors)
                False <$ logDebug msg
            VerSuccess -> do
                let fmt = "Block header " % build % " considered useful"
                    msg = sformat fmt h
                True <$ logDebug msg

handleBlockRequest
    :: ResponseMode m
    => RequestBlock SscDynamicState -> m ()
handleBlockRequest (RequestBlock h) = do
    logDebug $ sformat ("Block "%build%" is requested") h
    maybe logNotFound sendBlockBack =<< St.getBlock h
  where
    logNotFound = logDebug $ sformat ("Block "%build%" wasn't found") h
    sendBlockBack block = do
        statlogSentBlock block
        logDebug $ sformat ("Sending block "%build%" in reply") h
        replyToNode $ SendBlock block
