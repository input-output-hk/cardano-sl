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
import           Pos.Slotting (getSlotStart)
import           Pos.Txp (ProcessBlundsSettings (..), TxpBlund, TxpGlobalApplyMode,
                          TxpGlobalRollbackMode, TxpGlobalSettings (..), applyBlocksWith,
                          blundToAuxNUndo, processBlunds, txpGlobalSettings)
import           Pos.Util.Chrono (NewestFirst (..))
import qualified Pos.Util.Modifier as MM

import qualified Pos.Explorer.DB as GS
import           Pos.Explorer.Txp.Common (buildExplorerExtraLookup)
import           Pos.Explorer.Txp.Toil (EGlobalToilM, ExplorerExtraLookup (..),
                                        ExplorerExtraModifier (..), eApplyToil, eRollbackToil)

-- | Settings used for global transactions data processing used by explorer.
explorerTxpGlobalSettings :: HasConfiguration => TxpGlobalSettings
explorerTxpGlobalSettings =
    -- verification is same
    txpGlobalSettings
    { tgsApplyBlocks = applyBlocksWith applySettings
    , tgsRollbackBlocks = processBlunds rollbackSettings . getNewestFirst
    }

applySettings ::
       (TxpGlobalApplyMode ctx m, HasConfiguration)
    => ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
applySettings =
    ProcessBlundsSettings
        { pbsProcessSingle = applySingle
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = False
        }

rollbackSettings ::
       (TxpGlobalRollbackMode m, HasConfiguration)
    => ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
rollbackSettings =
    ProcessBlundsSettings
        { pbsProcessSingle = return . eRollbackToil . blundToAuxNUndo
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = True
        }

applySingle ::
       forall ctx m. (HasConfiguration, TxpGlobalApplyMode ctx m)
    => TxpBlund -> m (EGlobalToilM ())
applySingle txpBlund = do
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
                                  -- Genesis block doesn't have a slot, set to minBound
                                  }
            ComponentBlockMain mainHeader _  -> mainHeader ^. headerSlotL

    -- Get the timestamp from that information.
    mTxTimestamp <- getSlotStart slotId

    let (txAuxesAndUndos, hHash) = blundToAuxNUndoWHash txpBlund
    return $ eApplyToil mTxTimestamp txAuxesAndUndos hHash

extraOps :: HasConfiguration => ExplorerExtraModifier -> SomeBatchOp
extraOps (ExplorerExtraModifier em (HM.toList -> histories) balances utxoNewSum) =
    SomeBatchOp $
    map GS.DelTxExtra (MM.deletions em) ++
    map (uncurry GS.AddTxExtra) (MM.insertions em) ++
    map (uncurry GS.UpdateAddrHistory) histories ++
    map (uncurry GS.PutAddrBalance) (MM.insertions balances) ++
    map GS.DelAddrBalance (MM.deletions balances) ++
    map GS.PutUtxoSum (maybeToList utxoNewSum)

-- Zip block's TxAuxes and also add block hash
blundToAuxNUndoWHash :: TxpBlund -> ([(TxAux, TxUndo)], HeaderHash)
blundToAuxNUndoWHash blund@(blk, _) =
    (blundToAuxNUndo blund, headerHash blk)
