-- | Module containing explorer-specific logic and data

module Pos.DB.GState.Explorer
       ( ExplorerOp (..)
       , getTxExtra
       ) where

import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Explorer  ()
import           Pos.DB.Class         (MonadDB)
import           Pos.DB.Functions     (RocksBatchOp (..))
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.Txp.Core.Types   (TxId)
import           Pos.Types            (Address, HeaderHash, Timestamp)
import           Pos.Types.Explorer   (TxExtra (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxExtra :: MonadDB m => TxId -> m (Maybe TxExtra)
getTxExtra = gsGetBi . txExtraPrefix

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra

instance RocksBatchOp ExplorerOp where
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (encodeStrict extra)]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> encodeStrict h
