-- | Specification for transaction-related functions
-- (Pos.Types.Tx)

module Test.Pos.Types.TxSpec (spec) where

import           Test.Hspec            (Spec, describe, it, pendingWith)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Positive (..), arbitrary, forAll, resize)
import           Test.QuickCheck.Gen   (Gen)
import           Universum

import           Pos.Crypto            ()
import           Pos.Types             (Address (..), Tx (..), TxIn (..), TxOut (..),
                                        topsortTxs)

-- | Primitive transaction generator with restriction on
-- inputs/outputs size
txGen :: Int -> Gen Tx
txGen size = do
    (Positive inputsN) <- resize size arbitrary
    (Positive outputsN) <- resize size arbitrary
    inputs <- replicateM inputsN $ (\h s -> TxIn h 0 s) <$> arbitrary <*> arbitrary
    outputs <- replicateM outputsN $
        (\p (Positive c) -> TxOut (Address p) c) <$> arbitrary <*> (resize 100 arbitrary)
    pure $ Tx inputs outputs

spec :: Spec
spec = describe "Transaction topsort" $ do
    it "Returns [] on []" $ topsortTxs [] == (Just [])
    prop "Returns x for x" $ forAll (txGen 10) $ \x -> topsortTxs [x] == (Just [x])
    it "Doesn't change transaction set in general" $ pendingWith "not implemented"
    it "Does correct topsort for a graph" $ pendingWith "not implemented"
