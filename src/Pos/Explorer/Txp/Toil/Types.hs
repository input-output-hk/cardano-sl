{-# LANGUAGE TemplateHaskell #-}

-- | Additional types used by explorer's toil.

module Pos.Explorer.Txp.Toil.Types
       ( ExplorerExtra (..)
       , EToilModifier
       , eeLocalTxsExtra
       , eeAddrHistories
       , eeAddrBalances
       , ExplorerExtraTxp (..)
       ) where

import           Universum

import           Control.Lens      (makeLenses)
import           Data.Default      (Default, def)

import           Pos.Core          (Address, Coin)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Txp.Core      (TxId)
import           Pos.Txp.Toil      (GenericToilModifier)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory
type TxMapBalances = MM.MapModifier Address Coin

data ExplorerExtra = ExplorerExtra
    { _eeLocalTxsExtra :: !TxMapExtra
    , _eeAddrHistories :: !UpdatedAddrHistories
    , _eeAddrBalances  :: !TxMapBalances
    }

makeLenses ''ExplorerExtra

instance Default ExplorerExtra where
    def =
        ExplorerExtra
        { _eeLocalTxsExtra = mempty
        , _eeAddrHistories = mempty
        , _eeAddrBalances  = mempty
        }

type EToilModifier = GenericToilModifier ExplorerExtra

data ExplorerExtraTxp = ExplorerExtraTxp
    { eetTxExtra       :: !(HashMap TxId TxExtra)
    , eetAddrHistories :: !(HashMap Address AddrHistory)
    , eetAddrBalances  :: !(HashMap Address Coin)
    }
