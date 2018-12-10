-- | Transaction metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.TxMeta (
    -- * Transaction metadata
    module Types

  -- * Handy re-export to not leak our current choice of storage backend.
  , openMetaDB
  ) where

import           Universum

import           Control.Concurrent.MVar (withMVar)

import qualified Cardano.Wallet.Kernel.DB.Sqlite as ConcreteStorage
import           Cardano.Wallet.Kernel.DB.TxMeta.Types as Types

-- Concrete instantiation of 'MetaDBHandle'

openMetaDB :: FilePath -> IO MetaDBHandle
openMetaDB fp = do
    conn <- ConcreteStorage.newConnection fp
    lock <- newMVar conn
    return MetaDBHandle {
          closeMetaDB   = withMVar lock ConcreteStorage.closeMetaDB
        , migrateMetaDB = withMVar lock ConcreteStorage.unsafeMigrateMetaDB
        , clearMetaDB   = withMVar lock ConcreteStorage.clearMetaDB
        , deleteTxMetas = \w a   -> withMVar lock $ \c -> ConcreteStorage.deleteTxMetas c w a
        , getTxMeta     = \t w a -> withMVar lock $ \c -> ConcreteStorage.getTxMeta c t w a
        , putTxMeta     = \ t    -> withMVar lock $ \c -> ConcreteStorage.putTxMeta c t
        , putTxMetaT    = \ t    -> withMVar lock $ \c -> ConcreteStorage.putTxMetaT c t
        , getAllTxMetas = withMVar lock ConcreteStorage.getAllTxMetas
        , getTxMetas    = \o l af w t time s
                                 -> withMVar lock $ \c -> ConcreteStorage.getTxMetas c o l af w t time s
        }
