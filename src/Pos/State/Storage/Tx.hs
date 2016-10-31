{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Internal state of the transaction-handling worker.

module Pos.State.Storage.Tx
       (
         TxStorage
       , HasTxStorage(txStorage)
       , txStorageFromUtxo

       , getLocalTxs
       , getUtxoByDepth
       , txVerifyBlocks

       , processTx
       , txApplyBlocks
       , txRollback
       ) where

import           Control.Lens            (ix, makeClassy, preview, use, uses, view, (%=),
                                          (<~), (^.))
import           Data.Default            (Default, def)
import qualified Data.HashSet            as HS
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, int, sformat, (%))
import           Pos.Genesis             (genesisUtxo)
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types               (Block, SlotId, Tx (..), Utxo, applyTxToUtxo,
                                          blockSlot, blockTxs, slotIdF, verifyTxUtxo)
import           Serokell.Util           (VerificationRes (..), isVerSuccess)
import           Universum

data TxStorage = TxStorage
    { -- | Local set of transactions. These are valid (with respect to
      -- utxo) transactions which are known to the node and are not
      -- included in the blockchain store by the node.
      _txLocalTxs    :: !(HashSet Tx)
    , -- | Set of unspent transaction outputs. It is need to check new
      -- transactions and run follow-the-satoshi, for example.
      _txUtxo        :: !Utxo
    , -- | History of Utxo. May be necessary in case of
      -- reorganization. Also it is needed for MPC. Head of this list
      -- is utxo corresponding to last known block.
      _txUtxoHistory :: ![Utxo]
    }

makeClassy ''TxStorage
deriveSafeCopySimple 0 'base ''TxStorage

instance Default TxStorage where
    def =
        TxStorage
        { _txLocalTxs = mempty
        , _txUtxo = genesisUtxo
        , _txUtxoHistory = [genesisUtxo]
        }

-- | Generate TxStorage from non-default utxo
txStorageFromUtxo :: Utxo -> TxStorage
txStorageFromUtxo u = TxStorage mempty u [u]

type Query a = forall m x. (HasTxStorage x, MonadReader x m) => m a

getLocalTxs :: Query (HashSet Tx)
getLocalTxs = view txLocalTxs

txVerifyBlocks :: Word -> AltChain -> Query VerificationRes
txVerifyBlocks (fromIntegral -> toRollback) newChain = do
    mUtxo <- preview (txUtxoHistory . ix toRollback)
    return $ case mUtxo of
      Nothing ->
        VerFailure [sformat ("Can't rollback on "%int%" blocks") toRollback]
      Just utxo ->
        case foldM verifyDo utxo newChainTxs of
          Right _ -> VerSuccess
          Left es -> VerFailure es
  where
    newChainTxs =
        mconcat .
        fmap (\b -> fmap (b ^. blockSlot,) . toList $ b ^. blockTxs) . rights $
        NE.toList newChain
    verifyDo :: Utxo -> (SlotId, Tx) -> Either [Text] Utxo
    verifyDo utxo (slotId, tx) =
        case verifyTxUtxo utxo tx of
            VerSuccess    -> Right $ applyTxToUtxo tx utxo
            VerFailure es -> Left $ map (sformat eFormat tx slotId) es
    eFormat =
        "Failed to apply transaction ("%build%") on block from slot " %
        slotIdF%", error: "%build

-- | Get Utxo corresponding to state right after block with given
-- depth has been applied.
getUtxoByDepth :: Word -> Query (Maybe Utxo)
getUtxoByDepth (fromIntegral -> depth) = preview $ txUtxoHistory . ix depth

type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a

-- | Add transaction to storage if it is fully valid. Returns True iff
-- transaction has been added.
processTx :: Tx -> Update Bool
processTx tx = do
    good <- verifyTx tx
    good <$ when good (txLocalTxs %= HS.insert tx >> applyTx tx)

verifyTx :: Tx -> Update Bool
verifyTx tx = isVerSuccess . flip verifyTxUtxo tx <$> use txUtxo

applyTx :: Tx -> Update ()
applyTx tx = txUtxo %= applyTxToUtxo tx

removeLocalTx :: Tx -> Update ()
removeLocalTx tx = txLocalTxs %= HS.delete tx

-- | Apply chain of definitely valid blocks which go right after last
-- applied block.
txApplyBlocks :: AltChain -> Update ()
txApplyBlocks blocks = do
    mapM_ txApplyBlock blocks
    filterLocalTxs

txApplyBlock :: Block -> Update ()
txApplyBlock (Left _) = do
    utxo <- use txUtxo
    txUtxoHistory %= (utxo:)
txApplyBlock (Right mainBlock) = do
    utxo <- use txUtxo
    let txs = mainBlock ^. blockTxs
    mapM_ applyTx txs
    mapM_ removeLocalTx txs
    txUtxoHistory %= (utxo:)

-- | Rollback last `n` blocks. `tx` prefix is used, because rollback
-- may happen in other storages as well.
txRollback :: Word -> Update ()
txRollback (fromIntegral -> n) = do
    txUtxo <~ fromMaybe onError . (`atMay` n) <$> use txUtxoHistory
    txUtxoHistory %= drop n
    filterLocalTxs
  where
    -- Consider using `MonadError` and throwing `InternalError`.
    onError = (panic "attempt to rollback to too old or non-existing block")

filterLocalTxs :: Update ()
filterLocalTxs = do
    txs <- uses txLocalTxs toList
    txLocalTxs <~ HS.fromList <$> filterM verifyTx txs
