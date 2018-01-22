-- | Logic for global processing of transactions.
-- Global transaction is a transaction which has already been added to the blockchain.

module Pos.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ApplyBlocksSettings (..)
       , applyBlocksWith
       , blundToAuxNUndo
       , genericToilModifierToBatch
       , runToilAction
       ) where

import           Control.Monad.Except (runExceptT)
import           Data.Default (Default)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))
import           Universum

import           Pos.Core.Block.Union (ComponentBlock (..))
import           Pos.Core.Class (epochIndexL)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Txp (TxAux, TxUndo, TxpUndo)
import           Pos.DB (MonadDBRead, SomeBatchOp (..))
import           Pos.Exception (assertionFailed)
import           Pos.Txp.Base (flattenTxPayload)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil (DBToil, GenericToilModifier (..), GlobalApplyToilMode,
                               StakesView (..), ToilModifier, ToilT, applyToil, rollbackToil,
                               runDBToil, runToilTGlobal, verifyToil)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier as MM

-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: TxpGlobalSettings
txpGlobalSettings =
    TxpGlobalSettings
    { tgsVerifyBlocks = verifyBlocks
    , tgsApplyBlocks = applyBlocksWith applyBlocksSettings
    , tgsRollbackBlocks = rollbackBlocks
    }

verifyBlocks
    :: forall m.
       TxpGlobalVerifyMode m
    => Bool -> OldestFirst NE TxpBlock -> m (OldestFirst NE TxpUndo)
verifyBlocks verifyAllIsKnown newChain = do
    let epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
    fst <$> runToilAction @_ @() (mapM (verifyDo epoch) newChain)
  where
    verifyDo epoch = verifyToil epoch verifyAllIsKnown . convertPayload
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _ )     = []

data ApplyBlocksSettings extra m = ApplyBlocksSettings
    { absApplySingle     :: TxpBlund -> m ()
    , absExtraOperations :: extra -> SomeBatchOp
    }

applyBlocksSettings
    :: GlobalApplyToilMode m
    => ApplyBlocksSettings () m
applyBlocksSettings =
    ApplyBlocksSettings
    { absApplySingle = applyToil . blundToAuxNUndo
    , absExtraOperations = const mempty
    }

applyBlocksWith
    :: (TxpGlobalApplyMode ctx m, Default extra)
    => ApplyBlocksSettings extra (ToilT extra (DBToil m))
    -> OldestFirst NE TxpBlund
    -> m SomeBatchOp
applyBlocksWith ApplyBlocksSettings {..} blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- runExceptT $ verifyBlocks False blocks
        whenLeft verdict $
            assertionFailed .
            sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
    genericToilModifierToBatch absExtraOperations . snd <$>
        runToilAction (mapM absApplySingle blunds)

rollbackBlocks
    :: TxpGlobalRollbackMode m
    => NewestFirst NE TxpBlund -> m SomeBatchOp
rollbackBlocks blunds =
    toilModifierToBatch . snd <$>
    runToilAction (mapM (rollbackToil . blundToAuxNUndo) blunds)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GenericToilModifier' to batch of database operations.
genericToilModifierToBatch :: HasConfiguration
                           => (e -> SomeBatchOp)
                           -> GenericToilModifier e
                           -> SomeBatchOp
genericToilModifierToBatch convertExtra modifier =
    SomeBatchOp (extraOp : [SomeBatchOp utxoOps, SomeBatchOp stakesOps])
  where
    ToilModifier
        { _tmUtxo = um
        , _tmStakes = (StakesView (HM.toList -> stakes) total)
        , _tmExtra = extra
        } = modifier
    utxoOps =
        map DB.DelTxIn (MM.deletions um) ++
        map (uncurry DB.AddTxOut) (MM.insertions um)
    stakesOpsAlmost = map (uncurry DB.PutFtsStake) stakes
    stakesOps =
        case total of
            Nothing -> stakesOpsAlmost
            Just x  -> DB.PutTotalStake x : stakesOpsAlmost
    extraOp = convertExtra extra

-- | Convert simple 'ToilModifier' to batch of database operations.
toilModifierToBatch :: HasConfiguration => ToilModifier -> SomeBatchOp
toilModifierToBatch = genericToilModifierToBatch (const mempty)

-- | Run action which requires toil interfaces.
runToilAction
    :: (MonadDBRead m, Default e)
    => ToilT e (DBToil m) a -> m (a, GenericToilModifier e)
runToilAction action = runDBToil . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
