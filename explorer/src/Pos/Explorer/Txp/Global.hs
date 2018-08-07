-- | Explorer's global Txp (expressed as settings).

module Pos.Explorer.Txp.Global
       ( explorerTxpGlobalSettings
       , applySingle --TODO remove it from export list. It was added there to supress some -Wunused-imports
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Block (ComponentBlock (..), HeaderHash, headerHash,
                     headerSlotL)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core (HasConfiguration, SlotId (..), epochIndexL)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Txp (TxAux, TxUndo)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB (SomeBatchOp (..))
import           Pos.DB.Txp (ProcessBlundsSettings (..), TxpBlund,
                     TxpGlobalApplyMode, TxpGlobalRollbackMode,
                     TxpGlobalSettings (..), applyBlocksWith, blundToAuxNUndo,
                     processBlunds, txpGlobalSettings)
import           Pos.Infra.Slotting (getSlotStart)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed)

import qualified Pos.Explorer.DB as GS
import           Pos.Explorer.Txp.Common (buildExplorerExtraLookup)
import           Pos.Explorer.Txp.Toil (EGlobalToilM, ExplorerExtraLookup (..),
                     ExplorerExtraModifier (..), eApplyToil, eRollbackToil)
import           Pos.Explorer.Txp.Toil.Monad (ExplorerExtraM)

-- | Settings used for global transactions data processing used by explorer.
explorerTxpGlobalSettings :: HasConfiguration
                          => TraceNamed IO
                          -> ProtocolMagic
                          -> TxpConfiguration
                          -> TxpGlobalSettings
explorerTxpGlobalSettings logTrace pm txpConfig =
    -- verification is same
    (txpGlobalSettings pm txpConfig)
    { tgsApplyBlocks = \tr -> applyBlocksWith tr pm txpConfig (applySettings logTrace)
    , tgsRollbackBlocks = \_ -> processBlunds (rollbackSettings logTrace) . getNewestFirst
    }

applySettings ::
       TxpGlobalApplyMode ctx m
    => TraceNamed IO
    -> ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
applySettings _{-logTrace-} =
    ProcessBlundsSettings
        { pbsProcessSingle = --TODO \tx -> do
                                        -- applySingle logTrace tx
                                (const . return . return) ()
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = False
        }

rollbackSettings ::
       TxpGlobalRollbackMode m
    => TraceNamed IO
    -> ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
rollbackSettings _ =
    ProcessBlundsSettings
        { pbsProcessSingle = return . (eRollbackToil noTrace) . blundToAuxNUndo
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = True
        }

applySingle ::
       forall ctx m. (HasConfiguration, TxpGlobalApplyMode ctx m)
    => TraceNamed ExplorerExtraM
    -> TxpBlund
    -> m (EGlobalToilM ())
applySingle logTrace txpBlund = do
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
    return $ eApplyToil logTrace mTxTimestamp txAuxesAndUndos hHash

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
