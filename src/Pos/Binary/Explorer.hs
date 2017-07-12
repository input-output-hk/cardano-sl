-- | Binary instances for explorer types

module Pos.Binary.Explorer () where

import           Universum

import           Pos.Binary.Class        (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor         as Cbor
import           Pos.Binary.Txp          ()
import           Pos.Core                (HeaderHash, Timestamp)
import           Pos.Explorer.Core.Types (TxExtra (..))
import           Pos.Txp.Core            (TxOutAux)

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Field [| teReceivedTime    :: Timestamp                  |],
        Field [| teInputOutputs    :: NonEmpty TxOutAux          |]
    ]]

Cbor.deriveSimpleBi ''TxExtra [
    Cbor.Cons 'TxExtra [
        Cbor.Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Cbor.Field [| teReceivedTime    :: Timestamp                  |],
        Cbor.Field [| teInputOutputs    :: NonEmpty TxOutAux          |]
    ]]
