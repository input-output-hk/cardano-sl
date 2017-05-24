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
       ) where

import           Universum

import           Control.Lens        (_Wrapped)
import           Control.Monad.Catch (bracketOnError)
import           Data.List.NonEmpty  ((<|))
import qualified Ether
import           Formatting          (sformat, stext, (%))

import           Pos.Block.Core      (BlockHeader)
import           Pos.Constants       (slotSecurityParam)
import           Pos.Context         (BlkSemaphore, putBlkSemaphore, takeBlkSemaphore)
import           Pos.Core            (HeaderHash, diffEpochOrSlot, getEpochOrSlot,
                                      headerHash, prevBlockL)
import           Pos.Crypto          (shortHashF)
import           Pos.DB              (MonadDBPure)
import qualified Pos.DB.DB           as DB
import qualified Pos.DB.GState       as GS
import           Pos.Slotting.Class  (MonadSlots, getCurrentSlot)
import           Pos.Ssc.Class       (SscHelpersClass)
import           Pos.Util            (_neHead)
import           Pos.Util.Chrono     (NE, OldestFirst (getOldestFirst))

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
    :: (MonadDBPure m, SscHelpersClass ssc)
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
    :: Each [MonadIO, MonadMask, Ether.MonadReader' BlkSemaphore] '[m]
    => (HeaderHash -> m (a, HeaderHash)) -> m a
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = do
        (res, newTip) <- action tip
        res <$ putBlkSemaphore newTip

-- | Version of withBlkSemaphore which doesn't have any result.
withBlkSemaphore_
    :: Each [MonadIO, MonadMask, Ether.MonadReader' BlkSemaphore] '[m]
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
       (MonadSlots m, MonadDBPure m, SscHelpersClass ssc)
    => m Bool
needRecovery = maybe (pure True) isTooOld =<< getCurrentSlot
  where
    isTooOld currentSlot = do
        lastKnownBlockSlot <- getEpochOrSlot <$> DB.getTipHeader @ssc
        let distance = getEpochOrSlot currentSlot `diffEpochOrSlot`
                       lastKnownBlockSlot
        pure (distance > slotSecurityParam)
