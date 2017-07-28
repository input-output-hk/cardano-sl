{-# LANGUAGE TypeFamilies #-}

-- | Types used for managing of transactions
-- and synchronization with database.

module Pos.Txp.Toil.Types
       ( Utxo
       , utxoToStakes
       , formatUtxo
       , utxoF
       , GenesisUtxo (..)
       , GenesisStakeholders (..)
       , GenesisTxpContext
       , mkGenesisTxpContext
       , gtcUtxo
       , gtcStakeholders
       , _GenesisUtxo

       , TxFee(..)
       , MemPool (..)
       , mpLocalTxs
       , mpSize
       , TxMap
       , BalancesView (..)
       , bvStakes
       , bvTotal
       , UndoMap
       , UtxoModifier
       , fromUtxo
       , GenericToilModifier (..)
       , ToilModifier
       , tmUtxo
       , tmBalances
       , tmMemPool
       , tmUndos
       , tmExtra
       ) where

import           Universum

import           Control.Lens               (makeLenses, makePrisms, makeWrapped)
import           Data.Default               (Default, def)
import qualified Data.Map                   as M (toList)
import qualified Data.HashMap.Strict as HM
import           Data.Text.Lazy.Builder     (Builder)
import           Formatting                 (Format, later)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Text         (mapBuilderJson)

import           Pos.Core                   (Coin, StakeholderId, StakesMap, GenesisStakeholders (..),
                                             unsafeAddCoin)
import           Pos.Txp.Core               (TxAux, TxId, TxIn, TxOutAux, TxUndo, txOutStake)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Util              (getKeys)

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Convert 'Utxo' to 'StakesMap'.
utxoToStakes :: Utxo -> StakesMap
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, toaux) = foldl' plusAt hm (txOutStake toaux)

-- | Format 'Utxo' map for showing
formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

-- | Wrapper for genesis utxo.
newtype GenesisUtxo = GenesisUtxo
    { unGenesisUtxo :: Utxo
    } deriving (Show)

makePrisms  ''GenesisUtxo
makeWrapped ''GenesisUtxo

-- | Genesis context related to transaction processing.
data GenesisTxpContext = UnsafeGenesisTxpContext
    { _gtcUtxo         :: !GenesisUtxo
    , _gtcStakeholders :: !GenesisStakeholders
    }

makeLenses ''GenesisTxpContext

mkGenesisTxpContext :: GenesisUtxo -> GenesisTxpContext
mkGenesisTxpContext genUtxo = UnsafeGenesisTxpContext
    genUtxo
    (GenesisStakeholders . getKeys . utxoToStakes $ unGenesisUtxo genUtxo)

----------------------------------------------------------------------------
-- Fee
----------------------------------------------------------------------------

-- | tx.fee = sum(tx.in) - sum (tx.out)
newtype TxFee = TxFee Coin
    deriving (Show, Eq, Generic, Buildable)

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

data MemPool = MemPool
    { _mpLocalTxs :: !TxMap
      -- | Approximate size of encoded memory pool.
    , _mpSize     :: !Byte
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = mempty
        , _mpSize     = 1
        }

----------------------------------------------------------------------------
-- ToilModifier
----------------------------------------------------------------------------

type UtxoModifier = MM.MapModifier TxIn TxOutAux
type UndoMap = HashMap TxId TxUndo

fromUtxo :: Utxo -> UtxoModifier
fromUtxo = foldr (uncurry MM.insert) mempty . M.toList

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
