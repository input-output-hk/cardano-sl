{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

-- | Some utility functions necessary to implement block processing logic.

module Pos.Block.Logic.Util
       (
         -- * Common/Utils
         lcaWithMainChain
       , needRecovery
       , calcChainQuality
       , calcChainQualityM
       , calcOverallChainQuality
       , calcChainQualityFixedTime
       ) where

import           Universum

import           Control.Lens (_Wrapped)
import           Data.List (findIndex)
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import           Formatting (int, sformat, (%))
import           System.Wlog (WithLogger)

import           Pos.Block.Slog.Context (slogGetLastSlots)
import           Pos.Block.Slog.Types (HasSlogGState)
import           Pos.Core (BlockCount, FlatSlotId, HeaderHash, Timestamp (..), diffEpochOrSlot,
                           difficultyL, fixedTimeCQ, flattenSlotId, getEpochOrSlot, headerHash,
                           prevBlockL)
import           Pos.Core.Block (BlockHeader)
import           Pos.Core.Configuration (HasConfiguration, blkSecurityParam, slotSecurityParam)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadBlockDBRead)
import           Pos.Exception (reportFatalError)
import qualified Pos.GState as GS
import           Pos.Slotting (MonadSlots (..), getCurrentSlotFlat, slotFromTimestamp)
import           Pos.Util (_neHead)
import           Pos.Util.Chrono (NE, OldestFirst (..))

--- Usually in this method oldest header is LCA, so it can be optimized
-- by traversing from older to newer.
-- | Find LCA of headers list and main chain, including oldest
-- header's parent hash. Iterates from newest to oldest until meets
-- first header that's in main chain. O(n).
lcaWithMainChain
    :: (HasConfiguration, MonadBlockDBRead m)
    => OldestFirst NE BlockHeader -> m (Maybe HeaderHash)
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
needRecovery
    :: forall ctx m.
    ( HasConfiguration
    , MonadSlots ctx m
    , MonadBlockDBRead m
    )
    => m Bool
needRecovery = maybe (pure True) isTooOld =<< getCurrentSlot
  where
    isTooOld currentSlot = do
        lastKnownBlockSlot <- getEpochOrSlot <$> DB.getTipHeader
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
-- See documentation of 'chainQualityThreshold' to see why this
-- function returns any 'Fractional'.
calcChainQuality ::
       Fractional res => BlockCount -> FlatSlotId -> FlatSlotId -> Maybe res
calcChainQuality blockCount deepSlot newSlot
    | deepSlot == newSlot = Nothing
    | otherwise = Just $ realToFrac blockCount / realToFrac (newSlot - deepSlot)

-- | Version of 'calcChainQuality' which takes last blocks' slots from
-- the monadic context. It computes chain quality for last
-- 'blkSecurityParam' blocks.
calcChainQualityM ::
       ( MonadReader ctx m
       , HasSlogGState ctx
       , MonadIO m
       , MonadThrow m
       , WithLogger m
       , Fractional res
       , HasConfiguration
       )
    => FlatSlotId
    -> m (Maybe res)
calcChainQualityM newSlot = do
    OldestFirst lastSlots <- slogGetLastSlots
    let len = length lastSlots
    case nonEmpty lastSlots of
        Nothing -> return Nothing
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

-- | Calculate overall chain quality, i. e. number of main blocks
-- divided by number of slots so far. Returns 'Nothing' if current
-- slot is unknown.
calcOverallChainQuality ::
       forall ctx m res.
       (Fractional res, MonadSlots ctx m, MonadBlockDBRead m, HasConfiguration)
    => m (Maybe res)
calcOverallChainQuality =
    getCurrentSlotFlat >>= \case
        Nothing -> pure Nothing
        Just curFlatSlot ->
            calcOverallChainQualityDo curFlatSlot <$> DB.getTipHeader
  where
    calcOverallChainQualityDo curFlatSlot tipHeader
        | curFlatSlot == 0 = Nothing
        | otherwise =
            calcChainQuality
                (fromIntegral $ tipHeader ^. difficultyL)
                0
                curFlatSlot

-- | Calculate chain quality for approximately 'fixedTimeCQ'. Works
-- only if the following conditions are met:
--
-- 1. At least 'fixedTimeCQ' passed since system start.
-- 2. Block with depth 'blkSecurityParam' was created more than
-- 'fixedTimeCQ' ago. You should configure constants properly. For k =
-- 2160 'fixedTimeCQ' can be even 12h. We want 1h, so it's not
-- restrictive at all.
-- 3. We are able to determine which slot started 'fixedTimeCQ' ago.
calcChainQualityFixedTime ::
       forall ctx m res. (Fractional res, MonadSlots ctx m, HasConfiguration, HasSlogGState ctx)
    => m (Maybe res)
calcChainQualityFixedTime = do
    Timestamp curTime <- currentTimeSlotting
    let olderTime = Timestamp (curTime - fixedTimeCQ)
    (,) <$> slotFromTimestamp olderTime <*> getCurrentSlotFlat >>= \case
        (Just (flattenSlotId -> olderSlotId), Just currentSlotId) ->
            calcChainQualityFixedTimeDo olderSlotId currentSlotId <$>
            slogGetLastSlots
        _ -> return Nothing
  where
    -- 'lastSlots' contains slots of last 'k' blocks.
    -- We need to return 'Just' if we know now many blocks were created since
    -- 'olderSlotId'.
    -- We know it if there is a slot which is ≤ than 'olderSlotId' in
    -- 'lastSlots'.
    calcChainQualityFixedTimeDo ::
           FlatSlotId -> FlatSlotId -> OldestFirst [] FlatSlotId -> Maybe res
    calcChainQualityFixedTimeDo olderSlotId currentSlotId (OldestFirst lastSlots) =
        case findIndex (>= olderSlotId) lastSlots of
            Just firstNew
                | firstNew > 0 || head lastSlots == Just olderSlotId ->
                    let blockCount = fromIntegral (length lastSlots - firstNew)
                    in calcChainQuality blockCount olderSlotId currentSlotId
            -- All slots are less than 'olderSlotId', something is bad.
            _ -> Nothing
