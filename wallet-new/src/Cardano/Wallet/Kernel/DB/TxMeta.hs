-- | Transaction metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.TxMeta (
    -- * Transaction metadata
    module Types

  -- * Handy re-export to not leak our current choice of storage backend.
  , openMetaDB
  ) where

import qualified Cardano.Wallet.Kernel.DB.Sqlite as ConcreteStorage
import           Cardano.Wallet.Kernel.DB.TxMeta.Types as Types
import           Universum

-- Concrete instantiation of 'MetaDBHandle'

openMetaDB :: FilePath -> IO MetaDBHandle
openMetaDB fp = do
    conn <- ConcreteStorage.newConnection fp
    return MetaDBHandle {
          closeMetaDB   = ConcreteStorage.closeMetaDB conn
        , migrateMetaDB = ConcreteStorage.unsafeMigrateMetaDB conn
        , getTxMeta     = ConcreteStorage.getTxMeta conn
        , putTxMeta     = ConcreteStorage.putTxMeta conn
        , getTxMetas    = ConcreteStorage.getTxMetas conn
        }
