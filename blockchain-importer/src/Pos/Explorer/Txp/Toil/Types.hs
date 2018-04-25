-- | Additional types used by explorer's toil.

module Pos.Explorer.Txp.Toil.Types
       ( ExplorerExtraModifier (..)
       , eemLocalTxsExtra
       , eemAddrHistories
       , eemAddrBalances
       , eemNewUtxoSum
       , ExplorerExtraLookup (..)
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default, def)

import           Pos.Core (Address, Coin, TxId)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory
type TxMapBalances = MM.MapModifier Address Coin

-- | Modifier of extra data stored by explorer.
data ExplorerExtraModifier = ExplorerExtraModifier
    { _eemLocalTxsExtra :: !TxMapExtra
    , _eemAddrHistories :: !UpdatedAddrHistories
    , _eemAddrBalances  :: !TxMapBalances
    , _eemNewUtxoSum    :: !(Maybe Integer)
    }

makeLenses ''ExplorerExtraModifier

instance Default ExplorerExtraModifier where
    def =
        ExplorerExtraModifier
        { _eemLocalTxsExtra = mempty
        , _eemAddrHistories = mempty
        , _eemAddrBalances  = mempty
        , _eemNewUtxoSum    = Nothing
        }

-- | Functions to get extra data stored by explorer.
data ExplorerExtraLookup = ExplorerExtraLookup
    { eelGetTxExtra     :: TxId -> Maybe TxExtra
    , eelGetAddrHistory :: Address -> AddrHistory
    , eelGetAddrBalance :: Address -> Maybe Coin
    , eelGetUtxoSum     :: !Integer
    }
