{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Block processing related workers.

module Pos.Modern.Worker.Block
       (
       ) where

import           Control.Monad.State          (get)
import qualified Data.HashMap.Strict          as HM
import qualified Data.List.NonEmpty           as NE
import           Serokell.Util.Exceptions     ()
import           Universum

import           Pos.Binary.Communication     ()
import           Pos.Block.Logic              (applyBlocks, loadLastNBlocksWithUndo,
                                               rollbackBlocks, withBlkSemaphore)
import           Pos.Constants                (k)
import           Pos.Context                  (getNodeContext)
import           Pos.Context.Context          (ncSscLeaders, ncSscParticipants)
import           Pos.Modern.DB.Utxo           (iterateByUtxo)
import           Pos.Modern.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Ssc.Class.Helpers        (sscCalculateSeed)
import           Pos.Ssc.GodTossing.Functions (getThreshold)
import           Pos.Types                    (Address, Coin, Participants, SlotId (..),
                                               TxIn, TxOut (..))
import           Pos.WorkMode                 (WorkMode)

lpcOnNewSlot :: WorkMode ssc m => SlotId -> m () --Leaders and Participants computation
lpcOnNewSlot slotId@SlotId{..} = withBlkSemaphore $ \tip -> do
    blockUndos <- loadLastNBlocksWithUndo tip k
    rollbackBlocks blockUndos
    richmens <- getRichmens 10000 -- read T from config
    let threshold = getThreshold $ length richmens -- no, its wrong.....
    --mbSeed <- sscCalculateSeed siEpoch threshold -- SscHelperClassM needded
    let mbSeed = notImplemented
    leaders <-
        case mbSeed of
          Left e     -> panic "SSC couldn't compute seed"
          Right seed -> followTheSatoshi seed
    nc <- getNodeContext
    liftIO $ putMVar (ncSscLeaders nc) leaders
    liftIO $ putMVar (ncSscParticipants nc) richmens
    applyBlocks (map fst blockUndos)
    pure tip

-- | Second argument - T, min money. Int is too small, I guess
getRichmens :: forall ssc m . WorkMode ssc m => Coin -> m Participants
getRichmens moneyT =
    NE.fromList . HM.keys . HM.filter (>= moneyT) <$> execStateT (iterateByUtxo @ssc countMoneys) mempty
  where
    countMoneys :: (TxIn, TxOut) -> StateT (HM.HashMap Address Coin) m ()
    countMoneys (_, TxOut{..}) = do
        money <- get
        let val = HM.lookupDefault 0 txOutAddress money
        modify (HM.insert txOutAddress (val + txOutValue))
