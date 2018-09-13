-- | Explorer's global Txp (expressed as settings).

module Pos.Explorer.Txp.Global
       ( explorerTxpGlobalSettings
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Block (ComponentBlock (..), HeaderHash, headerHash,
                     headerSlotL)
import           Pos.Chain.Txp (TxAux, TxUndo, TxpConfiguration)
import           Pos.Core as Core (Config (..), SlotId (..),
                     configBootStakeholders, epochIndexL,
                     localSlotIndexMinBound)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Genesis (GenesisWStakeholders)
import           Pos.DB (SomeBatchOp (..))
import           Pos.DB.Txp (ProcessBlundsSettings (..), TxpBlund,
                     TxpGlobalApplyMode, TxpGlobalRollbackMode,
                     TxpGlobalSettings (..), applyBlocksWith, blundToAuxNUndo,
                     processBlunds, txpGlobalSettings)
import           Pos.Infra.Slotting (getSlotStart)
import qualified Pos.Util.Modifier as MM

import qualified Pos.Explorer.DB as GS
import           Pos.Explorer.Txp.Common (buildExplorerExtraLookup)
import           Pos.Explorer.Txp.Toil (EGlobalToilM, ExplorerExtraLookup (..),
                     ExplorerExtraModifier (..), eApplyToil, eRollbackToil)

-- | Settings used for global transactions data processing used by explorer.
explorerTxpGlobalSettings :: Core.Config
                          -> TxpConfiguration
                          -> TxpGlobalSettings
explorerTxpGlobalSettings coreConfig txpConfig =
    -- verification is same
    (txpGlobalSettings coreConfig txpConfig)
        { tgsApplyBlocks    = applyBlocksWith (configProtocolMagic coreConfig)
                                              txpConfig
                                              (applySettings bootStakeholders)
        , tgsRollbackBlocks = processBlunds (rollbackSettings bootStakeholders)
            . getNewestFirst
        }
    where bootStakeholders = configBootStakeholders coreConfig

applySettings ::
       (TxpGlobalApplyMode ctx m)
    => GenesisWStakeholders
    -> ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
applySettings bootStakeholders =
    ProcessBlundsSettings
        { pbsProcessSingle = applySingle bootStakeholders
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = False
        }

rollbackSettings ::
       (TxpGlobalRollbackMode m)
    => GenesisWStakeholders
    -> ProcessBlundsSettings ExplorerExtraLookup ExplorerExtraModifier m
rollbackSettings bootStakeholders =
    ProcessBlundsSettings
        { pbsProcessSingle = return . eRollbackToil bootStakeholders . blundToAuxNUndo
        , pbsCreateEnv = buildExplorerExtraLookup
        , pbsExtraOperations = extraOps
        , pbsIsRollback = True
        }

applySingle ::
       forall ctx m . TxpGlobalApplyMode ctx m
    => GenesisWStakeholders -> TxpBlund -> m (EGlobalToilM ())
applySingle bootStakeholders txpBlund = do
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
                                  , siSlot  = localSlotIndexMinBound
                                  -- Genesis block doesn't have a slot, set to minBound
                                  }
            ComponentBlockMain mainHeader _  -> mainHeader ^. headerSlotL

    -- Get the timestamp from that information.
    mTxTimestamp <- getSlotStart slotId

    let (txAuxesAndUndos, hHash) = blundToAuxNUndoWHash txpBlund
    return $ eApplyToil bootStakeholders mTxTimestamp txAuxesAndUndos hHash

extraOps :: ExplorerExtraModifier -> SomeBatchOp
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
