-- | Explorer's global Txp (expressed as settings).

module Pos.Explorer.Txp.Global
       ( explorerTxpGlobalSettings
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core (ComponentBlock (..), HasConfiguration, HeaderHash, SlotId (..),
                           epochIndexL, headerHash, headerSlotL)
import           Pos.Core.Txp (TxAux, TxUndo)
import           Pos.DB (SomeBatchOp (..))
import           Pos.Slotting (MonadSlots, getSlotStart)
import           Pos.Txp (ApplyBlocksSettings (..), TxpBlund, TxpGlobalRollbackMode,
                          TxpGlobalSettings (..), applyBlocksWith, blundToAuxNUndo,
                          genericToilModifierToBatch, runToilAction, txpGlobalSettings)
import           Pos.Util.Chrono (NE, NewestFirst (..))
import qualified Pos.Util.Modifier as MM

import qualified Pos.Explorer.DB as GS
import           Pos.Explorer.Txp.Toil (EGlobalApplyToilMode, ExplorerExtra (..), eApplyToil,
                                        eRollbackToil)



-- | Settings used for global transactions data processing used by explorer.
explorerTxpGlobalSettings :: HasConfiguration => TxpGlobalSettings
explorerTxpGlobalSettings =
    -- verification is same
    txpGlobalSettings
    { tgsApplyBlocks = applyBlocksWith eApplyBlocksSettings
    , tgsRollbackBlocks = rollbackBlocks
    }

eApplyBlocksSettings
    :: (HasConfiguration, EGlobalApplyToilMode m, MonadSlots ctx m)
    => ApplyBlocksSettings ExplorerExtra m
eApplyBlocksSettings =
    ApplyBlocksSettings
    { absApplySingle = applyBlund
    , absExtraOperations = extraOps
    }

extraOps :: HasConfiguration => ExplorerExtra -> SomeBatchOp
extraOps (ExplorerExtra em (HM.toList -> histories) balances utxoNewSum) =
    SomeBatchOp $
    map GS.DelTxExtra (MM.deletions em) ++
    map (uncurry GS.AddTxExtra) (MM.insertions em) ++
    map (uncurry GS.UpdateAddrHistory) histories ++
    map (uncurry GS.PutAddrBalance) (MM.insertions balances) ++
    map GS.DelAddrBalance (MM.deletions balances) ++
    map GS.PutUtxoSum (maybeToList utxoNewSum)

applyBlund
    :: (HasConfiguration, MonadSlots ctx m, EGlobalApplyToilMode m)
    => TxpBlund
    -> m ()
applyBlund txpBlund = do
    -- @TxpBlund@ is a block/blund with a reduced set of information required for
    -- transaction processing. We use it to determine at which slot did a transaction
    -- occur. TxpBlund has TxpBlock inside. If it's Left, it's a genesis block which
    -- doesn't contain transactions. It doesn't have a slot, only epoch, but you can
    -- use e. g. SlotId epoch minBound. If it's Right, you can use headerSlotL lens.
    --
    -- type TxpBlund = (TxpBlock, TxpUndo)
    -- type TxpBlock = ComponentBlock TxPayload

    let txpBlock = txpBlund ^. _1
    let slotId   = case txpBlock of
            ComponentBlockGenesis genesisBlock -> SlotId
                                  { siEpoch = genesisBlock ^. epochIndexL
                                  , siSlot  = minBound
                                  -- ^ Genesis block doesn't have a slot, set to minBound
                                  }
            ComponentBlockMain mainHeader _  -> mainHeader ^. headerSlotL

    -- Get the timestamp from that information.
    mTxTimestamp <- getSlotStart slotId

    uncurry (eApplyToil mTxTimestamp) $ blundToAuxNUndoWHash txpBlund

rollbackBlocks
    :: TxpGlobalRollbackMode m
    => NewestFirst NE TxpBlund -> m SomeBatchOp
rollbackBlocks blunds =
    (genericToilModifierToBatch extraOps) . snd <$>
    runToilAction (mapM (eRollbackToil . blundToAuxNUndo) blunds)

-- Zip block's TxAuxes and also add block hash
blundToAuxNUndoWHash :: TxpBlund -> ([(TxAux, TxUndo)], HeaderHash)
blundToAuxNUndoWHash blund@(blk, _) =
    (blundToAuxNUndo blund, headerHash blk)
