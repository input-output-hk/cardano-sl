{-# LANGUAGE TemplateHaskell #-}

module Pos.Txp.Txp.Types
       ( UtxoView (..)
       , uvAddUtxo
       , uvDelUtxo
       , MemPool (..)
       , TxMap
       , BalancesView (..)
       , bvStakes
       , bvTotal
       , TxpModifier (..)
       , txmUtxoView
       , txmBalances
       ) where

import           Control.Lens        (makeLenses)
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.Text           as T
import           Universum

import           Pos.Types           (Coin, StakeholderId, TxAux, TxId, TxIn, TxOutAux)

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

----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    { localTxs     :: !TxMap
      -- | @length@ is @O(n)@ for 'HM.HashMap' so we store it explicitly.
    , localTxsSize :: !Int
    }

instance Default MemPool where
    def =
        MemPool
        { localTxs = HM.empty
        , localTxsSize = 0
        }

----------------------------------------------------------------------------
-- TxpModifier
----------------------------------------------------------------------------

-- | Real data inside TxpLDHolder
data TxpModifier = TxpModifier
    { _txmUtxoView :: !UtxoView
    , _txmBalances :: !BalancesView
    }
makeLenses ''TxpModifier
