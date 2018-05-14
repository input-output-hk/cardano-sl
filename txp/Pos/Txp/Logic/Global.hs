{-# LANGUAGE TypeOperators #-}

-- | Logic for global processing of transactions.  Global transaction
-- is a transaction which has already been added to the blockchain.

module Pos.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ProcessBlundsSettings (..)
       , processBlunds
       , applyBlocksWith
       , blundToAuxNUndo
       ) where

import           Universum

import           Control.Lens (magnify, zoom)
import           Control.Monad.Except (throwError)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))

import           Pos.Core.Block.Union (ComponentBlock (..))
import           Pos.Core.Class (epochIndexL)
import           Pos.Core (HasCoreConfiguration, HasGenesisData, HasProtocolMagic)
import           Pos.Core.Txp (TxAux, TxUndo, TxpUndo)
import           Pos.DB (SomeBatchOp (..))
import           Pos.DB.Class (gsAdoptedBVData)
import qualified Pos.DB.GState.Stakes as DB
import           Pos.Exception (assertionFailed)
import           Pos.Txp.Base (flattenTxPayload)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Logic.Common (buildUtxo, buildUtxoForRollback)
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpCommonMode, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil (ExtendedGlobalToilM, GlobalToilEnv (..), GlobalToilM,
                               GlobalToilState (..), StakesView (..), ToilVerFailure, Utxo, UtxoM,
                               UtxoModifier, applyToil, defGlobalToilState, gtsUtxoModifier,
                               rollbackToil, runGlobalToilMBase, runUtxoM, utxoToLookup, verifyToil)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: (HasProtocolMagic, HasGenesisData) => TxpGlobalSettings
txpGlobalSettings =
    TxpGlobalSettings
    { tgsVerifyBlocks = verifyBlocks
    , tgsApplyBlocks = applyBlocksWith (processBlundsSettings False applyToil)
    , tgsRollbackBlocks = rollbackBlocks
    }

----------------------------------------------------------------------------
-- Verify
----------------------------------------------------------------------------

verifyBlocks ::
       forall m. (TxpGlobalVerifyMode m, HasProtocolMagic)
    => Bool
    -> OldestFirst NE TxpBlock
    -> m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
verifyBlocks verifyAllIsKnown newChain = runExceptT $ do
    bvd <- gsAdoptedBVData
    let verifyPure :: [TxAux] -> UtxoM (Either ToilVerFailure TxpUndo)
        verifyPure = runExceptT . verifyToil bvd epoch verifyAllIsKnown
        foldStep ::
               (UtxoModifier, [TxpUndo])
            -> TxpBlock
            -> ExceptT ToilVerFailure m (UtxoModifier, [TxpUndo])
        foldStep (modifier, undos) (convertPayload -> txAuxes) = do
            baseUtxo <- utxoToLookup <$> buildUtxo modifier txAuxes
            case runUtxoM modifier baseUtxo (verifyPure txAuxes) of
                (Left err, _) -> throwError err
                (Right txpUndo, newModifier) ->
                    return (newModifier, txpUndo : undos)
        -- 'NE.fromList' is safe here, because there will be at least
        -- one 'foldStep' (since 'newChain' is not empty) and it will
        -- either fail (and then 'convertRes' will not be called) or
        -- will prepend something to the result.
        convertRes :: (UtxoModifier, [TxpUndo]) -> OldestFirst NE TxpUndo
        convertRes = OldestFirst . NE.fromList . reverse . snd
    convertRes <$> foldM foldStep mempty newChain
  where
    epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _)      = []

----------------------------------------------------------------------------
-- General processing
----------------------------------------------------------------------------

data ProcessBlundsSettings extraEnv extraState m = ProcessBlundsSettings
    { pbsProcessSingle   :: TxpBlund -> m (ExtendedGlobalToilM extraEnv extraState ())
    , pbsCreateEnv       :: Utxo -> [TxAux] -> m extraEnv
    , pbsExtraOperations :: extraState -> SomeBatchOp
    , pbsIsRollback      :: !Bool
    -- ^ This flag specifies whether we want to rollback transactions
    -- or apply them. It affects the way we construct base 'Utxo'. If
    -- we want to apply transactions, we should use 'buildUtxo' to
    -- resolved all their inputs. But if we want to rollback them, we
    -- should turn known outputs of transactions into 'Utxo'.
    }

processBlunds ::
       forall extraEnv extraState m. (TxpCommonMode m, Default extraState)
    => ProcessBlundsSettings extraEnv extraState m
    -> NE TxpBlund
    -> m SomeBatchOp
processBlunds ProcessBlundsSettings {..} blunds = do
    let toBatchOp (gts, extra) =
            globalToilStateToBatch gts <> pbsExtraOperations extra
    totalStake <- DB.getRealTotalStake -- doesn't change
    -- Note: base utxo also doesn't change, but we build it on each
    -- step (for different sets of transactions), because
    -- 'UtxoModifier' may accumulate some data and it may be more
    -- efficient.

    -- Another note: if we rollback transactions, we don't really need
    -- base utxo, but we have a sanity check in 'utxoDel' which forces
    -- us to construct base utxo here.
    let buildBaseUtxo :: UtxoModifier -> [TxAux] -> m Utxo
        buildBaseUtxo
            | pbsIsRollback = buildUtxoForRollback
            | otherwise = buildUtxo

    let step ::
               (GlobalToilState, extraState)
            -> TxpBlund
            -> m (GlobalToilState, extraState)
        step st txpBlund = do
            processSingle <- pbsProcessSingle txpBlund
            let txAuxesAndUndos = blundToAuxNUndo txpBlund
                txAuxes = fst <$> txAuxesAndUndos
            baseUtxo <- buildBaseUtxo (st ^. _1 . gtsUtxoModifier) txAuxes
            extraEnv <- pbsCreateEnv baseUtxo txAuxes
            let gte =
                    GlobalToilEnv
                        { _gteUtxo = utxoToLookup baseUtxo
                        , _gteTotalStake = totalStake
                        }
            let env = (gte, extraEnv)
            runGlobalToilMBase DB.getRealStake . flip execStateT st .
                usingReaderT env $
                processSingle
    toBatchOp <$> foldM step (defGlobalToilState, def) blunds

----------------------------------------------------------------------------
-- Apply and rollback
----------------------------------------------------------------------------

applyBlocksWith ::
       forall extraEnv extraState ctx m.
       (TxpGlobalApplyMode ctx m, Default extraState, HasProtocolMagic)
    => ProcessBlundsSettings extraEnv extraState m
    -> OldestFirst NE TxpBlund
    -> m SomeBatchOp
applyBlocksWith settings blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- verifyBlocks False blocks
        whenLeft verdict $
            assertionFailed .
            sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
    processBlunds settings (getOldestFirst blunds)

processBlundsSettings ::
       forall m. Monad m
    => Bool
    -> ([(TxAux, TxUndo)] -> GlobalToilM ())
    -> ProcessBlundsSettings () () m
processBlundsSettings isRollback pureAction =
    ProcessBlundsSettings
        { pbsProcessSingle = \txpBlund -> pure (processSingle txpBlund)
        , pbsCreateEnv = \_ _ -> pure ()
        , pbsExtraOperations = const mempty
        , pbsIsRollback = isRollback
        }
  where
    processSingle :: TxpBlund -> ExtendedGlobalToilM () () ()
    processSingle = zoom _1 . magnify _1 . pureAction . blundToAuxNUndo

rollbackBlocks ::
       forall m. (TxpGlobalRollbackMode m, HasGenesisData)
    => NewestFirst NE TxpBlund
    -> m SomeBatchOp
rollbackBlocks (NewestFirst blunds) =
    processBlunds (processBlundsSettings True rollbackToil) blunds

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GlobalToilState' to batch of database operations.
globalToilStateToBatch :: HasCoreConfiguration => GlobalToilState -> SomeBatchOp
globalToilStateToBatch GlobalToilState {..} =
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp stakesOps]
  where
    StakesView (HM.toList -> stakes) total = _gtsStakesView
    utxoOps =
        map DB.DelTxIn (MM.deletions _gtsUtxoModifier) ++
        map (uncurry DB.AddTxOut) (MM.insertions _gtsUtxoModifier)
    stakesOps = addTotalStakeOp $ map (uncurry DB.PutFtsStake) stakes
    addTotalStakeOp =
        case total of
            Nothing -> identity
            Just x  -> (DB.PutTotalStake x :)

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
