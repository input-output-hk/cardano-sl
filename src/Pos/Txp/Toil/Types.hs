{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types used for managing of transactions
-- and synchronization with database.

module Pos.Txp.Toil.Types
       ( Utxo
       , formatUtxo
       , utxoF

       , MemPool (..)
       , mpLocalTxs
       , mpLocalTxsSize
       , TxMap
       , BalancesView (..)
       , bvStakes
       , bvTotal
       , UndoMap
       , UtxoModifier
       , GenericToilModifier (..)
       , ToilModifier
       , tmUtxo
       , tmBalances
       , tmMemPool
       , tmUndos
       , tmExtra

       -- * Env
       , ToilEnv (..)
       ) where

import           Control.Lens               (makeLenses)
import           Data.Default               (Default, def)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map                   as M (toList)
import           Data.Text.Lazy.Builder     (Builder)
import           Formatting                 (Format, later)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Text         (mapBuilderJson)
import           Universum

import           Pos.Core                   (Coin, StakeholderId)
import           Pos.Txp.Core               (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import qualified Pos.Util.Modifier          as MM

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Format 'Utxo' map for showing
formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

----------------------------------------------------------------------------
-- BalancesView
----------------------------------------------------------------------------

data BalancesView = BalancesView
    { _bvStakes :: !(HashMap StakeholderId Coin)
    , _bvTotal  :: !(Maybe Coin)
    }

makeLenses ''BalancesView

instance Default BalancesView where
    def = BalancesView mempty Nothing

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
        { _mpLocalTxs      = HM.empty
        , _mpLocalTxsSize  = 0
        }

----------------------------------------------------------------------------
-- ToilModifier
----------------------------------------------------------------------------

type UtxoModifier = MM.MapModifier TxIn TxOutAux
type UndoMap = HashMap TxId TxUndo

instance Default UndoMap where
    def = mempty

data GenericToilModifier extension = ToilModifier
    { _tmUtxo     :: !UtxoModifier
    , _tmBalances :: !BalancesView
    , _tmMemPool  :: !MemPool
    , _tmUndos    :: !UndoMap
    , _tmExtra    :: !extension
    }

type ToilModifier = GenericToilModifier ()

instance Default ext => Default (GenericToilModifier ext) where
    def = ToilModifier mempty def def mempty def

makeLenses ''GenericToilModifier

----------------------------------------------------------------------------
-- Toil environment
----------------------------------------------------------------------------

-- | Environment used by Toil.
data ToilEnv = ToilEnv
    { teMaxTxSize :: !Byte
    }
