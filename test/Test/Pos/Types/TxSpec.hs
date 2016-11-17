{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | Specification for transaction-related functions
-- (Pos.Types.Tx)
module Test.Pos.Types.TxSpec (spec) where

import           Test.Hspec            (Spec, describe, it, pendingWith)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..), Positive (..), arbitrary,
                                        forAll, resize, sized, vectorOf, (.&.), (===))
import           Test.QuickCheck.Gen   (Gen)
import           Universum             hiding ((.&.))

import           Pos.Crypto            (hash)
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

-- | Produces acyclic oriented graph of transactions. Shouldn't be
-- connected. Signatures are faked and thus fail to
-- verify. Transaction balance is bad too (input can be less than
-- output). These properties are not needed for topsort test.
txAcyclicGen :: Int -> Gen [Tx]
txAcyclicGen 0 = pure []
txAcyclicGen size = do
    initVertices <- replicateM (max 1 $ size `div` 2) (txGen 10)
    let outputs =
            concatMap
            (\tx -> map (hash tx,) $ [0..length (txOutputs tx) - 1])
            initVertices
    continueGraph initVertices outputs $ size - length initVertices
  where
    continueGraph vertices _ 0   = pure vertices
    continueGraph unusedUtxo _ k = do
        (NonNegative depsN) <-
            resize (max (length unusedUtxo) 3)
                   (arbitrary :: Gen (NonNegative Int))
        notImplemented

spec :: Spec
spec = describe "Transaction topsort" $ do
    prop "doesn't change the random set of transactions" $
        forAll (resize 10 $ arbitrary) $ \(NonNegative l) ->
        forAll (vectorOf l (txGen 10)) $ \txs ->
        (sort <$> topsortTxs txs) === Just (sort txs)
    it "does correct topsort for a acyclic graph" $ pendingWith "not implemented"
