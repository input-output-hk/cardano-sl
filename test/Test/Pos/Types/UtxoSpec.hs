-- | Specification of Pos.Types.Block.

module Test.Pos.Types.UtxoSpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe, it, pending)
import           Universum

spec :: Spec
spec = describe "Types.Utxo" $ do
    describe "findTxIn" $ do
        it "returns Nothing when given empty list" pending
