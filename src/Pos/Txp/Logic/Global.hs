{-# LANGUAGE ScopedTypeVariables #-}

-- | Settings used for global transactions data processing.

module Pos.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ApplyBlocksSettings (..)
       , applyBlocksWith
       , blundToAuxNUndo
       , genericToilModifierToBatch
       , runToilAction
       ) where

import           Control.Monad.Except    (runExceptT)
import           Data.Default            (Default)
import qualified Data.HashMap.Strict     as HM
import           Formatting              (build, sformat, (%))
import           Universum

import           Pos.DB                  (MonadDB, SomeBatchOp (..))
import qualified Pos.DB.GState           as GS
import           Pos.Exception           (assertionFailed)
import           Pos.Util                (NE, NewestFirst (..), OldestFirst (..),
                                          inAssertMode)
import qualified Pos.Util.Modifier       as MM

import           Pos.Txp.Core            (TxAux, TxUndo, TxpUndo, flattenTxPayload)
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil            (BalancesView (..), BalancesView (..), DBTxp,
                                          GenericToilModifier (..), GlobalToilMode,
                                          ToilModifier, ToilT, applyToil, rollbackToil,
                                          runDBTxp, runToilTGlobal, verifyToil)

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
    => OldestFirst NE TxpBlock -> m (OldestFirst NE TxpUndo)
verifyBlocks newChain =
    fst <$> runToilAction @_ @() (mapM (verifyToil . convertPayload) newChain)
  where
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (Left _)             = []
    convertPayload (Right (_, payload)) = flattenTxPayload payload

data ApplyBlocksSettings extra m = ApplyBlocksSettings
    { absApplySingle     :: TxpBlund -> m ()
    , absExtraOperations :: extra -> SomeBatchOp
    }

applyBlocksSettings
    :: forall m.
       GlobalToilMode m
    => ApplyBlocksSettings () m
applyBlocksSettings =
    ApplyBlocksSettings
    { absApplySingle = applyToil . blundToAuxNUndo
    , absExtraOperations = const mempty
    }

applyBlocksWith
    :: (TxpGlobalApplyMode m, Default extra)
    => ApplyBlocksSettings extra (ToilT extra (DBTxp m))
    -> OldestFirst NE TxpBlund
    -> m SomeBatchOp
applyBlocksWith ApplyBlocksSettings {..} blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- runExceptT $ verifyBlocks blocks
        whenLeft verdict $
            assertionFailed .
            sformat ("we are trying to apply txp blocks which we fail to verify :("%build)
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
genericToilModifierToBatch :: (e -> SomeBatchOp)
                           -> GenericToilModifier e
                           -> SomeBatchOp
genericToilModifierToBatch convertExtra ToilModifier { _tmUtxo = um
                                                     , _tmBalances = (BalancesView (HM.toList -> stakes) total)
                                                     , _tmExtra = extra
                                                     } =
    SomeBatchOp (extraOp : [SomeBatchOp utxoOps, SomeBatchOp balancesOps])
  where
    utxoOps =
        map GS.DelTxIn (MM.deletions um) ++
        map (uncurry GS.AddTxOut) (MM.insertions um)
    balancesOpsAlmost = map (uncurry GS.PutFtsStake) stakes
    balancesOps =
        case total of
            Nothing -> balancesOpsAlmost
            Just x  -> GS.PutFtsSum x : balancesOpsAlmost
    extraOp = convertExtra extra

-- | Convert simple 'ToilModifier' to batch of database operations.
toilModifierToBatch :: ToilModifier -> SomeBatchOp
toilModifierToBatch = genericToilModifierToBatch (const mempty)

-- | Run action which requires toil interfaces.
runToilAction
    :: (MonadDB m, Default e)
    => ToilT e (DBTxp m) a -> m (a, GenericToilModifier e)
runToilAction action = runDBTxp . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (Left _, _)                = []
blundToAuxNUndo (Right (_, payload), undo) = zip (flattenTxPayload payload) undo
