{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Block processing related workers.

module Pos.Worker.Block
       (
       ) where

import           Serokell.Util.Exceptions            ()
import           Universum

import           Pos.Binary.Communication            ()
import           Pos.Block.Logic                     (applyBlocks,
                                                      loadLastNBlocksWithUndo,
                                                      rollbackBlocks, withBlkSemaphore)
import           Pos.Constants                       (k)
import           Pos.Context                         (getNodeContext)
import           Pos.Context.Context                 (ncSscLeaders, ncSscParticipants)
import           Pos.Modern.FollowTheSatoshi         (followTheSatoshi)
import           Pos.Modern.Ssc.GodTossing.Functions (getThreshold)
import           Pos.Types                           (Participants, SlotId (..))
import           Pos.WorkMode                        (WorkMode)

lpcOnNewSlot :: WorkMode ssc m => SlotId -> m () --Leaders and Participants computation
lpcOnNewSlot slotId@SlotId{..} = withBlkSemaphore $ \tip -> do
    blockUndos <- loadLastNBlocksWithUndo tip k
    rollbackBlocks blockUndos
    participants <- getParticipants 0
    let threshold = getThreshold $ length participants
    --mbSeed <- sscCalculateSeed siEpoch threshold -- SscHelperClass needded
    let mbSeed = notImplemented
    leaders <-
        case mbSeed of
          Left e     -> panic "SSC couldn't compute seed"
          Right seed -> followTheSatoshi seed
    nc <- getNodeContext
    liftIO $ putMVar (ncSscLeaders nc) leaders
    liftIO $ putMVar (ncSscParticipants nc) participants
    applyBlocks (map fst blockUndos)
    pure tip

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants :: WorkMode ssc m => Int -> m Participants
getParticipants certs = notImplemented
    --iterator here
    -- mKeymap <- Just <$> view gsVssCertificates
    -- return $
    --     do keymap <- mKeymap
    --        let stakeholders =
    --                nub $ map txOutAddress (toList utxo)
    --        NE.nonEmpty $
    --            map vcVssKey $ mapMaybe (`HM.lookup` keymap) stakeholders
