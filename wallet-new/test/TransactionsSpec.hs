module TransactionsSpec where

import Universum

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Cardano.Wallet.API.V1.LegacyHandlers.Transactions

spec :: Spec
spec = do
    describe "feesIncluded distribution logic" $ do
        prop "distributeFeesInternal preserves amounts" $ \fee outputs ->
            case distributeFeesInternal fee outputs of
                Nothing ->
                    classify True "Inputs did not satisfy invariants" $
                    True === True
                Just distributed ->
                    sum (map fromIntegral outputs) - (fromIntegral fee)
                    ===
                    sum distributed
                    .&&.
                    all ((0 :: Integer) <) (map round distributed)

