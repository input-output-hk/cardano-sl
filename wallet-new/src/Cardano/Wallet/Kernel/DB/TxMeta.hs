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
  , getTxMetas
  , Limit (..)
  , Offset (..)
  , Sorting (..)
  , SortDirection (..)
  , SortCriteria (..)
  ) where

import           Cardano.Wallet.Kernel.DB.Sqlite (Limit (..), MetaDBHandle, Offset (..),
                                                  SortCriteria (..), SortDirection (..),
                                                  Sorting (..), closeMetaDB, getTxMeta, getTxMetas,
                                                  openMetaDB, putTxMeta)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types as Types

