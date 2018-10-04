module Pos.Core.Txp.TxAux
       ( TxAux (..)
       , txaF
       , checkTxAux
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Formatting (Format, bprint, build, later, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Text (listJsonIndent)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)

import           Pos.Core.Txp.Tx
import           Pos.Core.Txp.TxWitness

-- | Transaction + auxiliary data
data TxAux = TxAux
    { taTx      :: !Tx
    , taWitness :: !TxWitness
    } deriving (Generic, Show, Eq)

instance NFData TxAux

-- | Specialized formatter for 'TxAux'.
txaF :: Format r (TxAux -> r)
txaF = later $ \(TxAux tx w) ->
    bprint (build%"\n"%"witnesses: "%listJsonIndent 4) tx w

instance Buildable TxAux where
    build = bprint txaF

-- | Check that a 'TxAux' is internally valid (checks that its 'Tx' is valid
-- via 'checkTx'). Does not check the witness.
checkTxAux
    :: MonadError Text m
    => TxAux
    -> m ()
checkTxAux TxAux{..} = checkTx taTx

deriveSimpleBi ''TxAux [
    Cons 'TxAux [
        Field [| taTx       :: Tx        |],
        Field [| taWitness  :: TxWitness |]
    ]]
