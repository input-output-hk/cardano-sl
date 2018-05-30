{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}

-- | Global settings of Txp.

module Pos.Txp.Settings.Global
       ( TxpCommonMode
       , TxpGlobalVerifyMode
       , TxpGlobalApplyMode
       , TxpGlobalRollbackMode
       , TxpBlock
       , TxpBlund
       , TxpGlobalSettings (..)
       ) where

import           Universum

import           System.Wlog (WithLogger)

import           Pos.Core (ComponentBlock)
import           Pos.Core.Txp (TxPayload, TxpUndo)
import           Pos.DB (MonadDBRead, MonadGState, SomeBatchOp)
import           Pos.Slotting (MonadSlots)
import           Pos.Txp.Toil.Failure (ToilVerFailure)
import           Pos.Util.Chrono (NE, NewestFirst, OldestFirst)

type TxpCommonMode m =
    ( WithLogger m
    , MonadDBRead m
    , MonadGState m
    )

type TxpGlobalVerifyMode m =
    ( TxpCommonMode m
    )

type TxpGlobalApplyMode ctx m =
    ( TxpCommonMode m
    , MonadSlots ctx m  -- TODO: I don't like it (@gromak)
    )

type TxpGlobalRollbackMode m = TxpCommonMode m
type TxpBlock = ComponentBlock TxPayload
type TxpBlund = (TxpBlock, TxpUndo)

data TxpGlobalSettings = TxpGlobalSettings
    { -- | Verify a chain of payloads from blocks and return txp undos
      -- for each payload.
      --
      -- First argument determines whether it should be checked that
      -- all data from transactions is known (script versions,
      -- attributes, addresses, witnesses).
      tgsVerifyBlocks :: forall m. TxpGlobalVerifyMode m =>
                         Bool -> OldestFirst NE TxpBlock ->
                         m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
    , -- | Apply chain of /definitely/ valid blocks to Txp's GState.
      tgsApplyBlocks :: forall ctx m . TxpGlobalApplyMode ctx m =>
                        OldestFirst NE TxpBlund -> m SomeBatchOp
    , -- | Rollback chain of blocks.
      tgsRollbackBlocks :: forall m . TxpGlobalRollbackMode m =>
                           NewestFirst NE TxpBlund -> m SomeBatchOp
    }
