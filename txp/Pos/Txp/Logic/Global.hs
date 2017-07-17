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

import           Pos.DB                  (MonadDBRead, SomeBatchOp (..))
import           Pos.Exception           (assertionFailed)
import           Pos.Txp.Core            (TxAux, TxUndo, TxpUndo, flattenTxPayload)
import qualified Pos.Txp.DB              as DB
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil            (BalancesView (..), BalancesView (..), DBToil,
                                          GenericToilModifier (..), GlobalToilMode,
                                          ToilModifier, ToilT, applyToil, rollbackToil,
                                          runDBToil, runToilTGlobal, verifyToil)
import           Pos.Util.Chrono         (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier       as MM
import           Pos.Util.Util           (inAssertMode)

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
verifyBlocks verifyAllIsKnown newChain =
    fst <$> runToilAction @_ @() (mapM verifyDo newChain)
  where
    verifyDo = verifyToil verifyAllIsKnown . convertPayload
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
genericToilModifierToBatch :: (e -> SomeBatchOp)
                           -> GenericToilModifier e
                           -> SomeBatchOp
genericToilModifierToBatch convertExtra modifier =
    SomeBatchOp (extraOp : [SomeBatchOp utxoOps, SomeBatchOp balancesOps])
  where
    ToilModifier
        { _tmUtxo = um
        , _tmBalances = (BalancesView (HM.toList -> stakes) total)
        , _tmExtra = extra
        } = modifier
    utxoOps =
        map DB.DelTxIn (MM.deletions um) ++
        map (uncurry DB.AddTxOut) (MM.insertions um)
    balancesOpsAlmost = map (uncurry DB.PutFtsStake) stakes
    balancesOps =
        case total of
            Nothing -> balancesOpsAlmost
            Just x  -> DB.PutFtsSum x : balancesOpsAlmost
    extraOp = convertExtra extra

-- | Convert simple 'ToilModifier' to batch of database operations.
toilModifierToBatch :: ToilModifier -> SomeBatchOp
toilModifierToBatch = genericToilModifierToBatch (const mempty)

-- | Run action which requires toil interfaces.
runToilAction
    :: (MonadDBRead m, Default e)
    => ToilT e (DBToil m) a -> m (a, GenericToilModifier e)
runToilAction action = runDBToil . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (Left _, _)                = []
blundToAuxNUndo (Right (_, payload), undo) = zip (flattenTxPayload payload) undo
