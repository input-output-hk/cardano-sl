{-# LANGUAGE TypeFamilies #-}

-- | Types used for managing of transactions
-- and synchronization with database.

module Pos.Txp.Toil.Types
       ( Utxo
       , formatUtxo
       , utxoF
       , GenesisUtxo (..)
       , _GenesisUtxo

       , TxFee(..)
       , MemPool (..)
       , mpLocalTxs
       , mpSize
       , TxMap
       , StakesView (..)
       , svStakes
       , svTotal
       , UndoMap
       , UtxoModifier
       , AddrCoinMap
       , utxoToModifier
       , applyUtxoModToAddrCoinMap
       , GenericToilModifier (..)
       , ToilModifier
       , tmUtxo
       , tmStakes
       , tmMemPool
       , tmUndos
       , tmExtra
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms, makeWrapped)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M (lookup, member, toList)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, later)
import           Serokell.Util.Text (mapBuilderJson)

import           Pos.Core (Address, Coin, StakeholderId, unsafeAddCoin, unsafeSubCoin)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux (..), TxUndo, _TxOut)
import qualified Pos.Util.Modifier as MM

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

-- | Wrapper for genesis utxo.
newtype GenesisUtxo = GenesisUtxo
    { unGenesisUtxo :: Utxo
    } deriving (Show)

makePrisms  ''GenesisUtxo
makeWrapped ''GenesisUtxo

----------------------------------------------------------------------------
-- Fee
----------------------------------------------------------------------------

-- | tx.fee = sum(tx.in) - sum (tx.out)
newtype TxFee = TxFee Coin
    deriving (Show, Eq, Ord, Generic, Buildable)

----------------------------------------------------------------------------
-- StakesView
----------------------------------------------------------------------------

data StakesView = StakesView
    { _svStakes :: !(HashMap StakeholderId Coin)
    , _svTotal  :: !(Maybe Coin)
    }

makeLenses ''StakesView

instance Default StakesView where
    def = StakesView mempty Nothing

----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    { _mpLocalTxs :: !TxMap
      -- | Number of transactions in the memory pool.
    , _mpSize     :: !Int
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = mempty
        , _mpSize     = 0
        }

----------------------------------------------------------------------------
-- UtxoModifier, UndoMap and AddrCoinsMap
----------------------------------------------------------------------------

type UtxoModifier = MM.MapModifier TxIn TxOutAux
type UndoMap = HashMap TxId TxUndo
type AddrCoinMap = HashMap Address Coin

utxoToModifier :: Utxo -> UtxoModifier
utxoToModifier = foldl' (flip $ uncurry MM.insert) mempty . M.toList

-- | Takes utxo modifier and address-coin map with correspodning utxo
-- and applies utxo modifier to map.
-- Works for O(size of modifier * log (size of map)).
applyUtxoModToAddrCoinMap
    :: UtxoModifier
    -> (AddrCoinMap, Utxo)
    -> AddrCoinMap
applyUtxoModToAddrCoinMap modifier (addrCoins, utxo) = result
  where
    outToPair :: TxOutAux -> (Address, Coin)
    outToPair = view _TxOut . toaOut

    -- Resolve TxOut for every TxIn and convert TxOuts
    -- to pairs (Address, Coin)
    resolvedAddrs :: [(Address, Coin)]
    resolvedAddrs =
        mapMaybe (fmap outToPair . flip M.lookup utxo)
                 (MM.deletions modifier)

    -- subAddress and updateHM are used to do
    -- hashMap[address] = hashMap[address] - coins
    subAddress :: Coin -> Coin -> Maybe Coin
    subAddress r c = if r < c then Just (c `unsafeSubCoin` r) else Nothing

    updateHM :: HashMap Address Coin -> (Address, Coin) -> HashMap Address Coin
    updateHM hm (ad, coins) = HM.update (subAddress coins) ad hm

    -- Substract coins from current balances
    addrCoinsRest :: HashMap Address Coin
    addrCoinsRest = foldl' updateHM addrCoins resolvedAddrs

    -- Remove such TxIns which are already in wallet utxo.
    insertionsNotInUtxo :: [(TxIn, TxOutAux)]
    insertionsNotInUtxo = filter (not . flip M.member utxo . fst) (MM.insertions modifier)

    -- Convert TxOuts of insertionsNotInUtxo to [(Address, Coin)]
    addrCoinsAdditions :: [(Address, Coin)]
    addrCoinsAdditions = map (outToPair . snd) insertionsNotInUtxo

    -- Add coins to balances
    result :: HashMap Address Coin
    result = foldl' (flip $ uncurry $ HM.insertWith unsafeAddCoin) addrCoinsRest addrCoinsAdditions

instance Default UndoMap where
    def = mempty

----------------------------------------------------------------------------
-- ToilModifier
----------------------------------------------------------------------------

data GenericToilModifier extension = ToilModifier
    { _tmUtxo    :: !UtxoModifier
    , _tmStakes  :: !StakesView
    , _tmMemPool :: !MemPool
    , _tmUndos   :: !UndoMap
    , _tmExtra   :: !extension
    }

type ToilModifier = GenericToilModifier ()

instance Default ext => Default (GenericToilModifier ext) where
    def = ToilModifier mempty def def mempty def

makeLenses ''GenericToilModifier
