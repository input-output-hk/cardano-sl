module Pos.Core.Txp.TxAux
       ( TxAux (..)
       , txaF
       , checkTxAux
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, later, (%))
import           Serokell.Util.Text (listJsonIndent)

import           Pos.Binary.Class (Bi)

import           Pos.Core.Txp.Tx
import           Pos.Core.Txp.TxWitness

-- | Transaction + auxiliary data
data TxAux = TxAux
    { taTx      :: !Tx
    , taWitness :: !TxWitness
    } deriving (Generic, Show, Eq)

instance NFData TxAux

-- | Specialized formatter for 'TxAux'.
txaF :: Bi Tx => Format r (TxAux -> r)
txaF = later $ \(TxAux tx w) ->
    bprint (build%"\n"%"witnesses: "%listJsonIndent 4) tx w

instance Bi Tx => Buildable TxAux where
    build = bprint txaF

-- | Check that a 'TxAux' is internally valid (checks that its 'Tx' is valid
-- via 'checkTx'). Does not check the witness.
checkTxAux
    :: MonadError Text m
    => TxAux
    -> m ()
checkTxAux TxAux{..} = checkTx taTx
