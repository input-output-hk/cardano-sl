{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Pos.Wallet.State.Storage
       ( Storage (..)
       , Block'
       , HeaderHash'

       , Query
       , getBlock

       , getUtxo
       , getTxHistory

       , Update
       , blkSetHead
       ) where

import           Control.Lens                   (makeClassy)
import           Data.Default                   (Default, def)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HM
import           Data.SafeCopy                  (base, deriveSafeCopySimple)
import           Universum

import           Pos.Crypto                     (ProxyCert)
import           Pos.Types                      (Address, EpochIndex)
import           Pos.Wallet.State.Storage.Block (Block', BlockStorage,
                                                 HasBlockStorage (..), HeaderHash',
                                                 blkSetHead, getBlock)
import           Pos.Wallet.State.Storage.Tx    (HasTxStorage (..), TxStorage,
                                                 getTxHistory, getUtxo)

data Storage = Storage
    { -- Block-related part of wallet storage
      -- (partial keychain and alternative chains)
      __blockStorage :: BlockStorage
      -- Transaction-related part of wallet
      -- (Own Utxo with history + transactions history)
    , __txStorage    :: TxStorage
      -- Valid delegation certificates
    , _delegations   :: HashMap Address (ProxyCert (EpochIndex, EpochIndex))
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance Default Storage where
    def = Storage def def HM.empty

instance HasBlockStorage Storage where
    blockStorage = _blockStorage

instance HasTxStorage Storage where
    txStorage = _txStorage

type Query a = forall m. (MonadReader Storage m) => m a
type Update a = forall m. (MonadThrow m, MonadState Storage m) => m a
