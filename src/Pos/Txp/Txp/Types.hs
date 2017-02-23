{-# LANGUAGE TemplateHaskell #-}

-- | Types used for managing of transactions
-- and synchronization with database.

module Pos.Txp.Txp.Types
       ( UtxoView (..)
       , uvAddUtxo
       , uvDelUtxo
       , MemPool (..)
       , mpLocalTxs
       , mpLocalTxsSize
       , TxMap
       , BalancesView (..)
       , bvStakes
       , bvTotal
       , UndoMap
       , TxpModifier (..)
       , txmUtxoView
       , txmBalances
       , txmMemPool
       , txmUndos
       ) where

import           Control.Lens        (makeLenses)
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import           Universum

import           Pos.Types           (Coin, StakeholderId, TxAux, TxId, TxIn, TxOutAux,
                                      TxUndo, mkCoin)

----------------------------------------------------------------------------
-- UtxoView
----------------------------------------------------------------------------

data UtxoView = UtxoView
    { _uvAddUtxo :: !(HashMap TxIn TxOutAux)
    , _uvDelUtxo :: !(HashSet TxIn)
    }

makeLenses ''UtxoView

instance Default UtxoView where
    def = UtxoView mempty mempty

----------------------------------------------------------------------------
-- BalancesView
----------------------------------------------------------------------------

data BalancesView = BalancesView
    { _bvStakes :: !(HashMap StakeholderId Coin)
    , _bvTotal  :: !Coin
    }

makeLenses ''BalancesView

instance Default BalancesView where
    def = BalancesView mempty $ mkCoin 0

----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

instance Default TxMap where
    def = mempty

data MemPool = MemPool
    { _mpLocalTxs     :: !TxMap
      -- | @length@ is @O(n)@ for 'HM.HashMap' so we store it explicitly.
    , _mpLocalTxsSize :: !Int
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = HM.empty
        , _mpLocalTxsSize = 0
        }

----------------------------------------------------------------------------
-- TxpModifier
----------------------------------------------------------------------------

type UndoMap = HashMap TxId TxUndo

instance Default UndoMap where
    def = mempty

-- | Real data inside TxpLDHolder
data TxpModifier = TxpModifier
    { _txmUtxoView :: !UtxoView
    , _txmBalances :: !BalancesView
    , _txmMemPool  :: !MemPool
    , _txmUndos    :: !UndoMap
    }
makeLenses ''TxpModifier
