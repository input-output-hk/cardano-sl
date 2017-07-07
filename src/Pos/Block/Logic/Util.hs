{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Some utility functions necessary to implement block processing logic.

module Pos.Block.Logic.Util
       (
         -- * Common/Utils
         lcaWithMainChain
       , tipMismatchMsg
       , withBlkSemaphore
       , withBlkSemaphore_
       , needRecovery
       , calcChainQuality
       , calcChainQualityM
       ) where

import           Universum

import           Control.Lens           (_Wrapped)
import           Control.Monad.Catch    (bracketOnError)
import           Data.List.NonEmpty     ((<|))
import qualified Data.List.NonEmpty     as NE
import           Ether.Internal         (HasLens (..))
import           Formatting             (int, sformat, stext, (%))
import           System.Wlog            (WithLogger)

import           Pos.Block.Core         (BlockHeader)
import           Pos.Block.Slog.Context (slogGetLastSlots)
import           Pos.Block.Slog.Types   (HasSlogContext)
import           Pos.Constants          (blkSecurityParam, slotSecurityParam)
import           Pos.Context            (BlkSemaphore, putBlkSemaphore, takeBlkSemaphore)
import           Pos.Core               (BlockCount, FlatSlotId, HeaderHash,
                                         diffEpochOrSlot, getEpochOrSlot, headerHash,
                                         prevBlockL)
import           Pos.Crypto             (shortHashF)
import           Pos.DB                 (MonadDBRead)
import           Pos.DB.Block           (MonadBlockDB)
import qualified Pos.DB.DB              as DB
import qualified Pos.DB.GState          as GS
import           Pos.Exception          (reportFatalError)
import           Pos.Slotting.Class     (MonadSlots, getCurrentSlot)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Util               (_neHead)
import           Pos.Util.Chrono        (NE, OldestFirst (..))

-- | This function can be used to create a message when tip mismatch
-- is detected (usually between tip stored in DB and some other tip
-- received from somewhere).
tipMismatchMsg :: Text -> HeaderHash -> HeaderHash -> Text
tipMismatchMsg action storedTip attemptedTip =
    sformat
        ("Can't "%stext%" block because of tip mismatch (stored is "
         %shortHashF%", attempted is "%shortHashF%")")
        action storedTip attemptedTip

--- Usually in this method oldest header is LCA, so it can be optimized
-- by traversing from older to newer.
-- | Find LCA of headers list and main chain, including oldest
-- header's parent hash. Iterates from newest to oldest until meets
-- first header that's in main chain. O(n).
lcaWithMainChain
    :: (MonadDBRead m, SscHelpersClass ssc)
    => OldestFirst NE (BlockHeader ssc) -> m (Maybe HeaderHash)
lcaWithMainChain headers =
    lcaProceed Nothing $
    oldestParent <| fmap headerHash (getOldestFirst headers)
  where
    oldestParent :: HeaderHash
    oldestParent = headers ^. _Wrapped . _neHead . prevBlockL
    lcaProceed prevValue (h :| others) = do
        inMain <- GS.isBlockInMainChain h
        case (others, inMain) of
            (_, False)   -> pure prevValue
            ([], True)   -> pure $ Just h
            (x:xs, True) -> lcaProceed (Just h) (x :| xs)

-- | Run action acquiring lock on block application. Argument of
-- action is an old tip, result is put as a new tip.
withBlkSemaphore
    :: (MonadIO m, MonadMask m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => (HeaderHash -> m (a, HeaderHash)) -> m a
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = do
        (res, newTip) <- action tip
        res <$ putBlkSemaphore newTip

-- | Version of withBlkSemaphore which doesn't have any result.
withBlkSemaphore_
    :: (MonadIO m, MonadMask m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => (HeaderHash -> m HeaderHash) -> m ()
withBlkSemaphore_ = withBlkSemaphore . (fmap pure .)

-- | The phrase “we're in recovery mode” is confusing because it can mean two
-- different things:
--
-- 1. Last known block is more than K slots away from the current slot, or
--    current slot isn't known.
--
-- 2. We're actually in the process of requesting blocks because we have
--    detected that #1 happened. (See 'ncRecoveryHeader' and
--    'recoveryInProgress'.)
--
-- This function checks for #1. Note that even if we're doing recovery right
-- now, 'needRecovery' will still return 'True'.
--
needRecovery ::
       forall ssc m.
       (MonadSlots m, MonadBlockDB ssc m)
    => m Bool
needRecovery = maybe (pure True) isTooOld =<< getCurrentSlot
  where
    isTooOld currentSlot = do
        lastKnownBlockSlot <- getEpochOrSlot <$> DB.getTipHeader @ssc
        let distance = getEpochOrSlot currentSlot `diffEpochOrSlot`
                         lastKnownBlockSlot
        pure $ case distance of
            Just d  -> d > slotSecurityParam
            Nothing -> True   -- if current slot < last known slot, it's very
                              -- weird but at least we definitely know that
                              -- we don't need to do recovery

-- | Calculate chain quality using slot of the block which has depth =
-- 'blocksCount' and another slot after that one for which we
-- want to know chain quality.
--
-- 'Double' as the result type is fine, because this value is not part
-- of the protocol, we only need to compare it with 0.5 and 'Double'
-- is perfectly fine for this purpose. All other use cases are
-- heuristics which are implementation details of this application.
calcChainQuality :: BlockCount -> FlatSlotId -> FlatSlotId -> Double
calcChainQuality blockCount deepSlot newSlot =
    realToFrac blockCount / realToFrac (newSlot - deepSlot)

-- | Version of 'calcChainQuality' which takes last blocks' slots from
-- the monadic context.
calcChainQualityM ::
       ( MonadReader ctx m
       , HasSlogContext ctx
       , MonadIO m
       , MonadThrow m
       , WithLogger m
       )
    => FlatSlotId
    -> m Double
calcChainQualityM newSlot = do
    OldestFirst lastSlots <- slogGetLastSlots
    let len = length lastSlots
    case nonEmpty lastSlots of
        Nothing -> return 0
        Just slotsNE
            | len > fromIntegral blkSecurityParam ->
                reportFatalError $
                sformat ("number of last slots is greater than 'k': "%int) len
            | otherwise ->
                return
                    (calcChainQuality
                         (fromIntegral len)
                         (NE.head slotsNE)
                         newSlot)
