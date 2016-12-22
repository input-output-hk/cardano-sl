{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( lpcOnNewSlot
       ) where

import           Control.Monad.State      (get)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore)
import           Pos.Constants            (k)
import           Pos.Context              (getNodeContext)
import           Pos.Context.Context      (ncSscLeaders, ncSscRichmen)
import           Pos.FollowTheSatoshi     (followTheSatoshiM)
import           Pos.Modern.DB.Block      (loadBlocksWithUndoWhile)
import           Pos.Modern.DB.DBIterator ()
import           Pos.Modern.DB.Utxo       (getTotalCoins)
import           Pos.Modern.DB.Utxo       (iterateByUtxo, mapUtxoIterator)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (Address, Coin, EpochOrSlot (..), Participants,
                                           SlotId (..), TxIn, TxOut (..), getEpochOrSlot)
import           Pos.WorkMode             (WorkMode)

lpcOnNewSlot :: WorkMode ssc m => SlotId -> m () --Leaders and Participants computation
lpcOnNewSlot SlotId{siSlot = slotId, siEpoch = epochId} = withBlkSemaphore $ \tip -> do
    if slotId == 0 then do
        logDebug $ "It's time to compute leaders and parts"
        blockUndoList <- loadBlocksWithUndoWhile tip while5k
        when (null blockUndoList) $
            panic "No one block hasn't been generated during last k slots"
        let blockUndos = NE.fromList blockUndoList
        rollbackBlocks blockUndos
        -- [CSL-93] Use eligibility threshold here
        richmen <- getRichmen 0
        nc <- getNodeContext
        let clearMVar = liftIO . void . tryTakeMVar
        clearMVar $ ncSscRichmen nc
        liftIO $ putMVar (ncSscRichmen nc) richmen
        mbSeed <- sscCalculateSeed epochId
        totalCoins <- getTotalCoins
        leaders <-
            case mbSeed of
              Left e     -> panic $ sformat ("SSC couldn't compute seed: "%build) e
              Right seed -> mapUtxoIterator @(TxIn, TxOut) @TxOut
                            (followTheSatoshiM seed totalCoins) snd
        clearMVar $ ncSscLeaders nc
        liftIO $ putMVar (ncSscLeaders nc) leaders
        applyBlocks blockUndos
    else
        logDebug $ "It is too early compute leaders and parts"
    pure tip
  where
    while5k b = getEpochOrSlot b >= crucialSlot
    crucialSlot = EpochOrSlot $ Right $
                  if epochId == 0 then SlotId {siEpoch = 0, siSlot = 0}
                  else SlotId {siEpoch = epochId - 1, siSlot = 5 * k - 1}

-- | Second argument - T, min money.
getRichmen :: forall ssc m . WorkMode ssc m => Coin -> m Participants
getRichmen moneyT =
    fromMaybe onNoRichmen . NE.nonEmpty . HM.keys . HM.filter (>= moneyT) <$>
    execStateT (iterateByUtxo @ssc countMoneys) mempty
  where
    onNoRichmen = panic "There are no richmen!"
    countMoneys :: (TxIn, TxOut) -> StateT (HM.HashMap Address Coin) m ()
    countMoneys (_, TxOut {..}) = do
        money <- get
        let val = HM.lookupDefault 0 txOutAddress money
        modify (HM.insert txOutAddress (val + txOutValue))
