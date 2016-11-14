{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockListeners

       , handleBlock
       , handleBlockHeader
       , handleBlockRequest
       ) where

import           Control.TimeWarp.Logging  (logDebug, logInfo, logNotice, logWarning)
import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
import           Formatting                (build, int, sformat, stext, (%))
import           Serokell.Util             (VerificationRes (..), listJson)
import           Universum

import           Pos.Communication.Methods (announceBlock)
import           Pos.Communication.Types   (RequestBlock (..), ResponseMode,
                                            SendBlock (..), SendBlockHeader (..))
import           Pos.Communication.Util    (modifyListenerLogger)
import           Pos.Crypto                (hash)
import           Pos.DHT                   (ListenerDHT (..), replyToNode)
import           Pos.Slotting              (getCurrentSlot)
import qualified Pos.State                 as St
import           Pos.Statistics            (StatBlockCreated (..), statlogCountEvent)
import           Pos.Types                 (HeaderHash, getBlockHeader, headerHash)
import           Pos.Util.JsonLog          (jlAdoptedBlock, jlLog)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
               => [ListenerDHT m]
blockListeners =
    map (modifyListenerLogger "block")
        [ ListenerDHT handleBlock
        , ListenerDHT handleBlockHeader
        , ListenerDHT handleBlockRequest
        ]

handleBlock :: forall ssc m . ResponseMode ssc m
            => SendBlock ssc -> m ()
handleBlock (SendBlock block) = do
    slotId <- getCurrentSlot
    pbr <- St.processBlock slotId block
    let blkHash :: HeaderHash ssc
        blkHash = headerHash block
    case pbr of
        St.PBRabort msg -> do
            let fmt =
                    "Block "%build%
                    " processing is aborted for the following reason: "%stext
            logWarning $ sformat fmt blkHash msg
        St.PBRgood (0, (blkAdopted:|[])) -> do
            statlogCountEvent StatBlockCreated 1
            let adoptedBlkHash :: HeaderHash ssc
                adoptedBlkHash = headerHash blkAdopted
            jlLog $ jlAdoptedBlock blkAdopted
            logInfo $ sformat ("Received block has been adopted: "%build)
                adoptedBlkHash
        St.PBRgood (rollbacked, altChain) -> do
            statlogCountEvent StatBlockCreated 1
            logNotice $
                sformat ("As a result of block processing rollback of "%int%
                         " blocks has been done and alternative chain has been adopted "%
                         listJson)
                rollbacked (fmap headerHash altChain ::
                                   NonEmpty (HeaderHash ssc))
        St.PBRmore h -> do
            logInfo $ sformat
                ("After processing block "%build%", we need block "%build)
                blkHash h
            replyToNode $ RequestBlock h
    propagateBlock pbr

propagateBlock :: WorkMode ssc m
               => St.ProcessBlockRes ssc -> m ()
propagateBlock (St.PBRgood (_, blocks)) =
    either (const pass) announceBlock header
  where
    blk = NE.last blocks
    header = getBlockHeader blk
propagateBlock _ = pass

handleBlockHeader
    :: ResponseMode ssc m
    => SendBlockHeader ssc -> m ()

handleBlockHeader (SendBlockHeader header) = do
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
                        " for the following reasons: "%listJson
                let msg = sformat fmt h errors
                False <$ logDebug msg
            VerSuccess -> do
                let fmt = "Block header " % build % " considered useful"
                    msg = sformat fmt h
                True <$ logDebug msg

handleBlockRequest
    :: ResponseMode ssc m
    => RequestBlock ssc -> m ()
handleBlockRequest (RequestBlock h) = do
    logDebug $ sformat ("Block "%build%" is requested") h
    maybe logNotFound sendBlockBack =<< St.getBlock h
  where
    logNotFound = logWarning $ sformat ("Block "%build%" wasn't found") h
    sendBlockBack block = do
        logDebug $ sformat ("Sending block "%build%" in reply") h
        replyToNode $ SendBlock block
