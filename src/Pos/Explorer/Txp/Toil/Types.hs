{-# LANGUAGE TemplateHaskell #-}

-- | Additional types used by explorer's toil.

module Pos.Explorer.Txp.Toil.Types
       ( ExplorerExtra (..)
       , EToilModifier
       , eeLocalTxsExtra
       , eeAddrHistories
       ) where

import           Universum

import           Control.Lens      (makeLenses)
import           Data.Default      (Default, def)

import           Pos.Core          (Address)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Txp.Core      (TxId)
import           Pos.Txp.Toil      (GenericToilModifier)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory

data ExplorerExtra = ExplorerExtra
    { _eeLocalTxsExtra :: !TxMapExtra
    , _eeAddrHistories :: !UpdatedAddrHistories
    }

makeLenses ''ExplorerExtra

instance Default ExplorerExtra where
    def =
        ExplorerExtra
        { _eeLocalTxsExtra = mempty
        , _eeAddrHistories = mempty
        }

type EToilModifier = GenericToilModifier ExplorerExtra
