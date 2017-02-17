module Pos.Txp.Txp.Types
       ( UtxoView (..)
       , MemPool (..)
       , TxMap
       , BalancesView (..)
       , TxpModifier (..)
       ) where

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
    { addUtxo :: !(HashMap TxIn TxOutAux)
    , delUtxo :: !(HashSet TxIn)
    }

instance Default UtxoView where
    def = UtxoView mempty mempty

----------------------------------------------------------------------------
-- BalancesView
----------------------------------------------------------------------------

data BalancesView = BalancesView
    { _stakes :: !(HashMap StakeholderId Coin)
    , _total  :: !Coin
    }

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
    { txmUtxoView :: !UtxoView
    , txmMemPool  :: !MemPool
    , txmBalances :: !BalancesView
    }
