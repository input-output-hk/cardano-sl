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
       (
       ) where

import           Control.Monad.State          (get)
import qualified Data.HashMap.Strict          as HM
import qualified Data.List.NonEmpty           as NE
import           Serokell.Util.Exceptions     ()
import           System.Wlog                  (logDebug)
import           Universum

import           Pos.Binary.Communication     ()
import           Pos.Block.Logic              (applyBlocks, loadLastNBlocksWithUndo,
                                               rollbackBlocks, withBlkSemaphore)
import           Pos.Constants                (k)
import           Pos.Context                  (getNodeContext)
import           Pos.Context.Context          (ncSscLeaders, ncSscParticipants)
import           Pos.FollowTheSatoshi         (followTheSatoshiM)
import           Pos.Modern.DB.DBIterator     ()
import           Pos.Modern.DB.Utxo           (iterateByUtxo, mapUtxoIterator)
import           Pos.Ssc.GodTossing.Functions (getThreshold)
import           Pos.Types                    (Address, Coin, Participants, SlotId (..),
                                               TxIn, TxOut (..))
import           Pos.WorkMode                 (WorkMode)

lpcOnNewSlot :: WorkMode ssc m => SlotId -> m () --Leaders and Participants computation
lpcOnNewSlot SlotId{..} = withBlkSemaphore $ \tip -> do
    if siSlot == 0 then do
        logDebug $ "It's time to compute leaders and parts"
        blockUndos <- loadLastNBlocksWithUndo tip k
        rollbackBlocks blockUndos
        -- [CSL-93] Use eligibility threshold here
        richmen <- getRichmen 0
        let threshold = getThreshold $ length richmen -- no, its wrong.....
        --mbSeed <- sscCalculateSeed siEpoch threshold -- SscHelperClassM needded
        let mbSeed = notImplemented
        leaders <-
            case mbSeed of
              Left e     -> panic "SSC couldn't compute seed"
              Right seed -> mapUtxoIterator @(TxIn, TxOut) @TxOut
                            (followTheSatoshiM seed notImplemented) snd --balance
        nc <- getNodeContext
        let clearMVar = liftIO . void . tryTakeMVar
        clearMVar $ ncSscLeaders nc
        clearMVar $ ncSscParticipants nc
        liftIO $ putMVar (ncSscLeaders nc) leaders
        liftIO $ putMVar (ncSscParticipants nc) richmen
        applyBlocks (map fst blockUndos)
    else
        logDebug $ "It is too early compute leaders and parts"
    pure tip

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
