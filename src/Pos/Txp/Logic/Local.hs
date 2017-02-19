{-# LANGUAGE ConstraintKinds #-}

module Pos.Txp.Logic.Local
    ( txProcessTransaction
    , txNormalize
    ) where

import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.HashMap.Strict       as HM
import           System.Wlog               (WithLogger)
import           Universum

import           Pos.DB.Class              (MonadDB)
import           Pos.Txp.MemState          (MonadTxpMem (..))
import           Pos.Txp.Txp.Failure       (TxpVerFailure)
import           Pos.Txp.Txp.Types         (MemPool (..))
import           Pos.Types                 (TxAux, TxId)


type TxpLocalWorkMode ssc m =
    ( MonadDB ssc m
    , MonadTxpMem m
    , WithLogger m
    , MonadError TxpVerFailure m
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode ssc m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, (tx, _, _)) = notImplemented
  --   tipBefore <- getTip
  --   -- резолвим инпуты
  --   -- run current . run зарезовленные
  --   resolved <-
  --     foldM (\s inp -> maybe s (\x -> HM.insert inp x s) <$> utxoGet inp)
  --           mempty (txInputs tx)
  --   txpMem <- askTxpMem
  --   --pRes <- atomically $ do
  --   --pRes <- modifyTxpLD $ \ ->
  --   logDebug (sformat ("Transaction processed: "%build) txId)
  --   case pRes of
  --       Left fail -> throwError fail
  --       Right _   -> pure ()
  -- where
  --   processTxDo tipBefore txld@(_, MemPool{..}, _, tip)
  --       | tipBefore /= tip            = (Left $ TxpInvalid "Tips aren't same", txld)
  --       | localTxsSize >= maxLocalTxs = (Left $ TxpOverwhelmed, txld)
  --       | otherwise = case runExcept (processTx txld) of
  --           er@(Left _)  -> (er, txld)
  --           ok@(Right _) -> (ok, notImplemented)

    -- inputResolver tin
    --     | HS.member tin delUtxo = Nothing
    --     | otherwise =
    --         maybe (HM.lookup tin addUtxo) Just (HM.lookup tin resolvedIns)



-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
txNormalize :: (MonadDB ssc m, MonadTxpMem m)
               => m ()
txNormalize = notImplemented
