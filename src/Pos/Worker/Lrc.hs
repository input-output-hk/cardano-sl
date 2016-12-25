{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Worker.Lrc
       ( lrcOnNewSlot
       ) where
import           Control.Monad.State      (get)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore_)
import           Pos.Constants            (k)
import           Pos.Context              (getNodeContext)
import           Pos.Context.Context      (ncSscLeaders, ncSscRichmen)
import           Pos.Crypto               (PublicKey)
import           Pos.DB                   (loadBlocksFromTipWhile)
import           Pos.DB.DBIterator        ()
import           Pos.DB.Utxo              (getTotalCoins, iterateByUtxo, mapUtxoIterator)
import           Pos.FollowTheSatoshi     (followTheSatoshiM)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (Address, Coin, EpochOrSlot (..), HeaderHash,
                                           Participants, SlotId (..), TxIn, TxOut (..),
                                           TxOutAux, getEpochOrSlot, txOutStake)
import           Pos.Types.Address        (AddressHash)
import           Pos.WorkMode             (WorkMode)

lrcOnNewSlot :: WorkMode ssc m => SlotId -> m ()
lrcOnNewSlot slotId
    | siSlot slotId < k = do
        nc <- getNodeContext
        richmenEmpty <- liftIO . isEmptyMVar . ncSscRichmen $ nc
        leadersEmpty <- liftIO . isEmptyMVar . ncSscLeaders $ nc
        when (richmenEmpty || leadersEmpty) $
            withBlkSemaphore_ $ lrcOnNewSlotDo slotId
    | otherwise = do
        nc <- getNodeContext
        let clearMVar = liftIO . void . tryTakeMVar
        clearMVar $ ncSscRichmen nc
        clearMVar $ ncSscLeaders nc

lrcOnNewSlotDo
    :: WorkMode ssc m
    => SlotId -> HeaderHash ssc -> m (HeaderHash ssc)
lrcOnNewSlotDo SlotId {siEpoch = epochId} tip = tip <$ do
    logDebug $ "It's time to compute leaders and parts"
    blockUndoList <- loadBlocksFromTipWhile whileMoreOrEq5k
    when (null blockUndoList) $
        panic "No one block hasn't been generated during last k slots"
    let blockUndos = NE.fromList blockUndoList
    rollbackBlocks blockUndos
    nc <- getNodeContext
    let richmenMVar = ncSscRichmen nc
        leadersMVar = ncSscLeaders nc
    whenM (liftIO $ isEmptyMVar richmenMVar) $ do
        -- [CSL-93] Use eligibility threshold here
        richmen <- getRichmen 0
        liftIO $ putMVar richmenMVar richmen
    whenM (liftIO $ isEmptyMVar leadersMVar) $ do
        mbSeed <- sscCalculateSeed epochId
        totalCoins <- getTotalCoins
        leaders <-
            case mbSeed of
                Left e     -> panic $ sformat ("SSC couldn't compute seed: "%build) e
                Right seed -> mapUtxoIterator @(TxIn, TxOutAux) @TxOutAux
                              (followTheSatoshiM seed totalCoins) snd
        liftIO $ putMVar leadersMVar leaders
    applyBlocks blockUndos
  where
    whileMoreOrEq5k b = getEpochOrSlot b >= crucialSlot
    crucialSlot = EpochOrSlot $ Right $
                  if epochId == 0 then SlotId {siEpoch = 0, siSlot = 0}
                  else SlotId {siEpoch = epochId - 1, siSlot = 5 * k - 1}

-- | Get nodes which have enough stake to participate in SSC.
getRichmen
    :: forall ssc m.
       WorkMode ssc m
    => Coin                  -- ^ Eligibility threshold
    -> m Participants
getRichmen moneyT =
    fromMaybe onNoRichmen . NE.nonEmpty . HM.keys . HM.filter (>= moneyT) <$>
    execStateT (iterateByUtxo @ssc countMoneys) mempty
  where
    onNoRichmen = panic "There are no richmen!"
    countMoneys :: (TxIn, TxOutAux)
                -> StateT (HM.HashMap (AddressHash PublicKey) Coin) m ()
    countMoneys (_, txo) = for_ (txOutStake txo) $ \(a, c) -> do
        money <- get
        let val = HM.lookupDefault 0 a money
        modify (HM.insert a (val + c))
