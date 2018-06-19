module Pos.Core.Txp.TxOutAux
       ( TxOutAux (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Core.Txp.Tx (TxOut)

-- | Transaction output and auxilary data corresponding to it.
-- [CSL-366] Add more data.
data TxOutAux = TxOutAux
    { toaOut   :: !TxOut
    -- ^ Tx output
    } deriving (Generic, Show, Eq, Ord)

instance Buildable TxOutAux where
    build (TxOutAux out) = bprint ("{txout = "%build%"}") out

instance NFData TxOutAux
