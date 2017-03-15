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
#ifdef WITH_EXPLORER
       , mpLocalTxsExtra
       , mpAddrHistories
#endif
       , TxMap
       , BalancesView (..)
       , bvStakes
       , bvTotal
       , UndoMap
       , UtxoModifier
       , ToilModifier (..)
       , tmUtxo
       , tmBalances
       , tmMemPool
       , tmUndos

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
#ifdef WITH_EXPLORER
import           Pos.Core                   (Address)
import           Pos.Types.Explorer         (AddrHistory, TxExtra)
#endif

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Format 'Utxo' map as json.
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

#ifdef WITH_EXPLORER
type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory

instance Default TxMapExtra where
    def = mempty

instance Default UpdatedAddrHistories where
    def = mempty
#endif

data MemPool = MemPool
    { _mpLocalTxs      :: !TxMap
      -- | @length@ is @O(n)@ for 'HM.HashMap' so we store it explicitly.
    , _mpLocalTxsSize  :: !Int
#ifdef WITH_EXPLORER
    , _mpLocalTxsExtra :: !TxMapExtra
    , _mpAddrHistories :: !UpdatedAddrHistories
#endif
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs      = HM.empty
        , _mpLocalTxsSize  = 0
#ifdef WITH_EXPLORER
        , _mpLocalTxsExtra = def
        , _mpAddrHistories = def
#endif
        }

----------------------------------------------------------------------------
-- ToilModifier
----------------------------------------------------------------------------

type UtxoModifier = MM.MapModifier TxIn TxOutAux
type UndoMap = HashMap TxId TxUndo

instance Default UndoMap where
    def = mempty

data ToilModifier = ToilModifier
    { _tmUtxo     :: !UtxoModifier
    , _tmBalances :: !BalancesView
    , _tmMemPool  :: !MemPool
    , _tmUndos    :: !UndoMap
    }

instance Default ToilModifier where
    def = ToilModifier mempty def def mempty

makeLenses ''ToilModifier

----------------------------------------------------------------------------
-- Toil environment
----------------------------------------------------------------------------

-- | Environment used by Toil.
data ToilEnv = ToilEnv
    { teMaxTxSize :: !Byte
    }
