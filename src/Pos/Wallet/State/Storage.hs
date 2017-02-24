{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Wallet.State.Storage
       ( Storage (..)
       , Block'

       , mkStorage

       , Query
       , getBlock
       , getBestChain

       , getUtxo
       , getOldestUtxo
       , getTxHistory

       , getSlotDuration
       , getMaxBlockSize

       , Update
       , blkSetHead
       ) where

import           Control.Lens                   (makeClassy)
import           Data.Default                   (def)
import           Data.SafeCopy                  (base, deriveSafeCopySimple)
import           Data.Time.Units                (Millisecond)
import           Serokell.Data.Memory.Units     (Byte)
import           Universum

import qualified Pos.Constants                  as Const
import           Pos.Crypto                     (ProxyCert)
import           Pos.Txp.Core                   (Utxo)
import           Pos.Types                      (Address, EpochIndex)

import           Pos.Wallet.State.Storage.Block (Block', BlockStorage,
                                                 HasBlockStorage (..), blkSetHead,
                                                 getBestChain, getBlock)
import           Pos.Wallet.State.Storage.Tx    (HasTxStorage (..), TxStorage,
                                                 getOldestUtxo, getTxHistory, getUtxo,
                                                 mkTxStorage)

data Storage = Storage
    { -- Block-related part of wallet storage
      -- (partial keychain and alternative chains)
      __blockStorage :: BlockStorage
      -- Transaction-related part of wallet
      -- (Own Utxo with history + transactions history)
    , __txStorage    :: TxStorage
      -- Valid delegation certificates
    , _delegations   :: HashMap Address (ProxyCert (EpochIndex, EpochIndex))
      -- “Constants”
    , _slotDuration  :: Millisecond
    , _maxBlockSize  :: Byte
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

mkStorage :: Utxo -> Storage
mkStorage u =
    Storage
        def
        (mkTxStorage u)
        mempty
        Const.genesisSlotDuration
        Const.genesisMaxBlockSize

instance HasBlockStorage Storage where
    blockStorage = _blockStorage

instance HasTxStorage Storage where
    txStorage = _txStorage

type Query a = forall m. (MonadReader Storage m) => m a
type Update a = forall m. (MonadThrow m, MonadState Storage m) => m a

getSlotDuration :: Query Millisecond
getSlotDuration = view slotDuration

getMaxBlockSize :: Query Byte
getMaxBlockSize = view maxBlockSize
