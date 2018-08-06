{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}

-- | Utilities for finding LCA and calculating chain quality.

module Pos.DB.Block.Logic.Util
       (
         -- * Common/Utils
         lcaWithMainChain
       , lcaWithMainChainSuffix
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

import           Pos.Chain.Block (HasBlockConfiguration, HasSlogGState,
                     fixedTimeCQ)
import           Pos.Core (BlockCount, FlatSlotId, HasProtocolConstants,
                     Timestamp (..), difficultyL, flattenSlotId)
import           Pos.Core.Block (BlockHeader, HeaderHash, headerHash,
                     prevBlockL)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Configuration (blkSecurityParam)
import           Pos.Core.Exception (traceFatalError)
import           Pos.Core.Slotting (MonadSlots (..), getCurrentSlotFlat,
                     slotFromTimestamp)
import           Pos.DB.Block.GState.BlockExtra (isBlockInMainChain)
import           Pos.DB.Block.Slog.Context (slogGetLastSlots)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadBlockDBRead)
import           Pos.Util (_neHead)
import           Pos.Util.Trace.Named (TraceNamed)

-- | Find LCA of headers list and main chain, including oldest
-- header's parent hash. Acts as it would iterate from newest to
-- oldest until it meets the first header in the main chain (which is
-- O(n)).
--
-- Though, usually in this method oldest header is LCA, so it can be
-- optimized by traversing from older to newer.
lcaWithMainChain
    :: ( MonadBlockDBRead m )
    => OldestFirst NE BlockHeader -> m (Maybe HeaderHash)
lcaWithMainChain headers =
    lcaProceed Nothing $
    oldestParent <| fmap headerHash (getOldestFirst headers)
  where
    oldestParent :: HeaderHash
    oldestParent = headers ^. _Wrapped . _neHead . prevBlockL
    lcaProceed prevValue (h :| others) = do
        inMain <- isBlockInMainChain h
        case (others, inMain) of
            (_, False)   -> pure prevValue
            ([], True)   -> pure $ Just h
            (x:xs, True) -> lcaProceed (Just h) (x :| xs)

-- | Split the input list into those which are in the main chain (given by the
-- 'MonadBlockDBRead' constraint), and those which are not. Those in the
-- chain are given NewestFirst.
lcaWithMainChainSuffix
    :: forall m .
       (MonadBlockDBRead m)
    => OldestFirst [] HeaderHash
    -> m (NewestFirst [] HeaderHash, OldestFirst [] HeaderHash)
lcaWithMainChainSuffix headers = go [] (getOldestFirst headers)
  where
    go :: [HeaderHash]
       -> [HeaderHash]
       -> m (NewestFirst [] HeaderHash, OldestFirst [] HeaderHash)
    -- Everything is in the chain.
    go !acc [] = pure (NewestFirst acc, OldestFirst [])
    go !acc (hh:rest) = do
        inMain <- isBlockInMainChain hh
        case inMain of
            False -> pure (NewestFirst acc, OldestFirst (hh : rest))
            True  -> go (hh:acc) rest

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
       , Fractional res
       , HasProtocolConstants
       )
    => TraceNamed m
    -> FlatSlotId
    -> m (Maybe res)
calcChainQualityM logTrace newSlot = do
    OldestFirst lastSlots <- slogGetLastSlots
    let len = length lastSlots
    case nonEmpty lastSlots of
        Nothing -> return Nothing
        Just slotsNE
            | len > fromIntegral blkSecurityParam ->
                traceFatalError logTrace $
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
       (Fractional res, MonadSlots ctx m, MonadBlockDBRead m)
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
       forall ctx m res.
       ( Fractional res
       , MonadSlots ctx m
       , HasBlockConfiguration
       , HasSlogGState ctx
       , HasProtocolConstants
       )
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
    -- We know it if there is a slot which is <= than 'olderSlotId' in
    -- 'lastSlots'.
    calcChainQualityFixedTimeDo ::
           FlatSlotId -> FlatSlotId -> OldestFirst [] FlatSlotId -> Maybe res
    calcChainQualityFixedTimeDo olderSlotId currentSlotId (OldestFirst lastSlots) =
        case findIndex (>= olderSlotId) lastSlots of
            Just firstNew
                | firstNew > 0 || fmap fst (uncons lastSlots) == Just olderSlotId ->
                    let blockCount = fromIntegral (length lastSlots - firstNew)
                    in calcChainQuality blockCount olderSlotId currentSlotId
            -- All slots are less than 'olderSlotId', something is bad.
            _ -> Nothing
