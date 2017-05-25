{-# LANGUAGE Rank2Types #-}

-- | Global settings of Txp.

module Pos.Txp.Settings.Global
       ( TxpGlobalVerifyMode
       , TxpGlobalApplyMode
       , TxpGlobalRollbackMode
       , TxpBlock
       , TxpBlund
       , TxpGlobalSettings (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           System.Wlog          (WithLogger)

import           Pos.Core             (IsGenesisHeader, IsMainHeader)
import           Pos.DB               (MonadDB, MonadDBPure, SomeBatchOp)
import           Pos.Slotting         (MonadSlots)
import           Pos.Util             (Some)
import           Pos.Util.Chrono      (NE, NewestFirst, OldestFirst)

import           Pos.Txp.Core         (TxPayload, TxpUndo)
import           Pos.Txp.Toil.Failure (ToilVerFailure)

type TxpGlobalVerifyMode m = ( WithLogger m
                             , MonadDB m
                             , MonadDBPure m
                             , MonadError ToilVerFailure m
                             )

type TxpGlobalApplyMode m = ( WithLogger m
                            , MonadDB m
                            , MonadDBPure m
                            , MonadSlots m  -- TODO: I don't like it (@gromak)
                            )

type TxpGlobalRollbackMode m = ( WithLogger m
                               , MonadDB m
                               , MonadDBPure m
                               )

-- [CSL-1156] Maybe find better approach (at least wrap into normal types).
type TxpBlock = Either (Some IsGenesisHeader) (Some IsMainHeader, TxPayload)
type TxpBlund = (TxpBlock, TxpUndo)

data TxpGlobalSettings = TxpGlobalSettings
    { -- | Verify a chain of payloads from blocks and return txp undos
      -- for each payload.
      --
      -- First argument determines whether it should be checked that
      -- all data from transactions is known (script versions,
      -- attributes, addresses, witnesses).
      tgsVerifyBlocks :: forall m. TxpGlobalVerifyMode m =>
                         Bool -> OldestFirst NE TxpBlock -> m (OldestFirst NE TxpUndo)
    , -- | Apply chain of /definitely/ valid blocks to Txp's GState.
      tgsApplyBlocks :: forall m . TxpGlobalApplyMode m =>
                        OldestFirst NE TxpBlund -> m SomeBatchOp
    , -- | Rollback chain of blocks.
      tgsRollbackBlocks :: forall m . TxpGlobalRollbackMode m =>
                           NewestFirst NE TxpBlund -> m SomeBatchOp
    }
