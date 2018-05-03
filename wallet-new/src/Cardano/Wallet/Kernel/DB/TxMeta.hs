-- | Transaction metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.TxMeta (
    -- * Transaction metadata
    module Types

  -- * Handy re-exports to not leak our current choice of storage backend.
  , MetaDBHandle
  , openMetaDB
  , closeMetaDB
  , getTxMeta
  , putTxMeta
  ) where

import           Cardano.Wallet.Kernel.DB.Sqlite (MetaDBHandle, closeMetaDB, getTxMeta, openMetaDB,
                                                  putTxMeta)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types as Types

