{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.DB.Compression (
    DeltaCheckpoint (..)
  , DeltaPartialCheckpoint (..)
  , UtxoDiff
  , BlockMetaDiff
  , BlockMetaSlotIdDiff
  , BlockMetaAddressDiff
  ) where

import           Universum

import qualified Data.SafeCopy as SC

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (PendingDiff)
import           UTxO.Util

import           Test.Pos.Core.Arbitrary ()

-- This module aims to help define memory efficient Safecopy instances for Checkpoints.
-- This is done by defining new types, which are the diffs of the Checkpoints.
-- To achieve this we first define diff types for the building blocks of Checkpoint:
-- Map, Pending, BlockMeta and eventually Checkpoint.

data DeltaCheckpoint = DeltaCheckpoint {
    dcUtxo        :: !(InDb UtxoDiff)
  , dcUtxoBalance :: !(InDb Core.Coin)
  , dcPending     :: !PendingDiff
  , dcBlockMeta   :: !BlockMetaDiff
  , dcForeign     :: !PendingDiff
  , dcContext     :: !(Maybe BlockContext)
}

data DeltaPartialCheckpoint = DeltaPartialCheckpoint {
    dcpUtxo        :: !(InDb UtxoDiff)
  , dcpUtxoBalance :: !(InDb Core.Coin)
  , dcpPending     :: !PendingDiff
  , dcpBlockMeta   :: !BlockMetaDiff
  , dcpForeign     :: !PendingDiff
  , dcpContext     :: !BlockContext
}

type UtxoDiff = MapDiff Core.TxIn Core.TxOutAux
type BlockMetaDiff = (BlockMetaSlotIdDiff, BlockMetaAddressDiff)
type BlockMetaSlotIdDiff = InDb (MapDiff Core.TxId Core.SlotId)
type BlockMetaAddressDiff = MapDiff (InDb Core.Address) AddressMeta

instance Differentiable (InDb Core.Utxo) (InDb UtxoDiff) where
  findDelta  = findDeltaUtxo
  applyDelta = applyDeltaUtxo

findDeltaUtxo :: InDb Core.Utxo -> InDb Core.Utxo -> InDb UtxoDiff
findDeltaUtxo inm inm' = findDelta <$> inm <*> inm'

applyDeltaUtxo :: InDb Core.Utxo -> InDb UtxoDiff -> InDb Core.Utxo
applyDeltaUtxo inu dinu = applyDelta <$> inu <*> dinu

instance Differentiable BlockMeta BlockMetaDiff where
  findDelta = findDeltaBlockMeta
  applyDelta = applyDeltaBlockMeta

findDeltaBlockMeta :: BlockMeta -> BlockMeta -> BlockMetaDiff
findDeltaBlockMeta (BlockMeta bmsi bma) (BlockMeta bmsi' bma') =
  (findDelta <$> bmsi <*> bmsi', findDelta bma bma')

applyDeltaBlockMeta :: BlockMeta -> BlockMetaDiff -> BlockMeta
applyDeltaBlockMeta (BlockMeta bmsi bms) (dbmsi, dbms) =
  BlockMeta (applyDelta <$> bmsi <*> dbmsi) (applyDelta bms dbms)

SC.deriveSafeCopy 1 'SC.base ''DeltaCheckpoint
SC.deriveSafeCopy 1 'SC.base ''DeltaPartialCheckpoint
