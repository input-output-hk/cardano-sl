-- | Additional types used by explorer's toil.

module Pos.Explorer.Txp.Toil.Types
       ( ExplorerExtra (..)
       , EToilModifier
       , eeLocalTxsExtra
       , eeAddrHistories
       , eeAddrBalances
       , eeNewUtxoSum
       , ExplorerExtraTxp (..)
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default, def)

import           Pos.Core (Address, Coin, TxId)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Txp.Toil (GenericToilModifier)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory
type TxMapBalances = MM.MapModifier Address Coin

data ExplorerExtra = ExplorerExtra
    { _eeLocalTxsExtra :: !TxMapExtra
    , _eeAddrHistories :: !UpdatedAddrHistories
    , _eeAddrBalances  :: !TxMapBalances
    , _eeNewUtxoSum    :: !(Maybe Integer)
    }

makeLenses ''ExplorerExtra

instance Default ExplorerExtra where
    def =
        ExplorerExtra
        { _eeLocalTxsExtra = mempty
        , _eeAddrHistories = mempty
        , _eeAddrBalances  = mempty
        , _eeNewUtxoSum    = Nothing
        }

type EToilModifier = GenericToilModifier ExplorerExtra

data ExplorerExtraTxp = ExplorerExtraTxp
    { eetTxExtra       :: !(HashMap TxId TxExtra)
    , eetAddrHistories :: !(HashMap Address AddrHistory)
    , eetAddrBalances  :: !(HashMap Address Coin)
    , eetUtxoSum       :: !Integer
    }
