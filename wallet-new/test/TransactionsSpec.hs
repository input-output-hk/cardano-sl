module TransactionsSpec where

import Universum

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

import Cardano.Wallet.API.V1.LegacyHandlers.Transactions

spec :: Spec
spec = do
    describe "feesIncluded distribution logic" $ do
        prop "distributeFeesInternal preserves amounts" $ \fee outputs' ->
            let
                -- I suspect there were some infinite lists causing
                -- nontermination. Or possibly some absolutely massive 'Integer'
                -- values doing the same.
                outputs = NE.fromList (NE.take 1000 outputs')
                distr = distributeFeesInternal fee outputs
             in
                cover (isJust distr) 75 "Inputs did not satisfy invariants" $
                case distr of
                    Nothing ->
                        True === True
                    Just distributed ->
                        let expected =
                                sum (map toInteger outputs) - toInteger fee
                            actual =
                                sum (map toInteger distributed)
                            difference =
                                abs (expected - actual)
                         in counterexample
                            ("Difference was: " <> show difference)
                            $ difference <= 5
                              .&&.
                              all (0 <) distributed
                              -- due to rounding error, we currently observe
                              -- a maximum difference of 5 Lovelace between what
                              -- we expect and the actual value. This amount
                              -- would go to extra fees.

        it "in case of fee that divides to X.5, goes to both" $ do
            let outputs = 21 :| [21]
                fee = 1
            distributeFeesInternal fee outputs
                `shouldBe`
                    Just (20 :| [20])
