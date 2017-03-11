-- | Specification for transaction-related core functions
-- (Pos.Txp.Core).

module Test.Pos.Txp.CoreSpec
       ( spec
       ) where

import           Universum

import           Control.Lens          (each)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (elemIndex, (\\))
import qualified Data.List.NonEmpty    as NE
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..), Positive (..), Property,
                                        arbitrary, forAll, resize, shuffle, vectorOf,
                                        (.&.), (===))
import           Test.QuickCheck.Gen   (Gen)

import           Pos.Crypto            (hash, whData, withHash)
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Txp.Core          (Tx (..), TxIn (..), TxOut (..), mkTx, topsortTxs)
import           Pos.Types             (mkCoin)
import           Pos.Util              (sublistN, _neHead)

spec :: Spec
spec = describe "Txp.Core" $ do
    describe "mkTx" $ do
        prop description_mkTxGood mkTxGood
        prop description_mkTxBad mkTxBad
    describe "topsortTxs" $ do
        prop "doesn't change the random set of transactions" $
            forAll (resize 10 $ arbitrary) $ \(NonNegative l) ->
            forAll (vectorOf l (txGen 10)) $ \txs ->
            (sort <$> topsortTxs identity (map withHash txs)) === Just (sort $ map withHash txs)
        prop "graph generator does not produce loops" $
            forAll (txAcyclicGen False 20) $ \(txs,_) ->
            forAll (shuffle $ map withHash txs) $ \shuffled ->
            isJust $ topsortTxs identity shuffled
        prop "does correct topsort on bamboo" $ testTopsort True
        prop "does correct topsort on arbitrary acyclic graph" $ testTopsort False
  where
    description_mkTxGood =
        "creates Tx if arguments are taken from valid Tx"
    description_mkTxBad =
        "doesn't create Tx with non-positive coins in outputs"

mkTxGood :: Tx -> Bool
mkTxGood UnsafeTx{..} = isJust $ mkTx _txInputs _txOutputs _txAttributes

mkTxBad :: Tx -> Bool
mkTxBad UnsafeTx {..} =
    all (\outs -> isNothing $ mkTx _txInputs outs _txAttributes) badOutputs
  where
    invalidateOut :: TxOut -> TxOut
    invalidateOut out = out {txOutValue = mkCoin 0}
    badOutputs :: [NonEmpty TxOut]
    badOutputs = [ _txOutputs & _neHead %~ invalidateOut
                 , _txOutputs & each %~ invalidateOut
                 ]

testTopsort :: Bool -> Property
testTopsort isBamboo =
    forAll (txAcyclicGen isBamboo 40) $ \(txs,reach) ->
    forAll (shuffle txs) $ \shuffled ->
    let reachables :: [(Tx,Tx)]
        reachables = [(from,to) | (to,froms) <- HM.toList reach, from <- froms]
        topsorted = map whData <$> topsortTxs identity (map withHash shuffled)
        reaches :: (Tx,Tx) -> Bool
        reaches (from,to) =
            let fromI = elemIndex from =<< topsorted
                toI = elemIndex to =<< topsorted
            in Just True == ((<=) <$> fromI <*> toI)
    in isJust topsorted .&. all reaches reachables

-- | Produces acyclic oriented graph of transactions. It's
-- connected. Signatures are faked and thus fail to
-- verify. Transaction balance is bad too (input can be less than
-- output). These properties are not needed for topsort test. It also
-- returns reachability map as the second argument (for every key
-- elems from which we can reach key).
txAcyclicGen :: Bool -> Int -> Gen ([Tx], HM.HashMap Tx [Tx])
txAcyclicGen _ 0 = pure ([], HM.empty)
txAcyclicGen isBamboo size = do
    initVertices <-
        replicateM (bool (max 1 $ size `div` 4) 1 isBamboo) $ txGen some'
    let outputs =
            concatMap (\tx -> map (tx,) [0..length (_txOutputs tx) - 1])
                      initVertices
        reachable = HM.fromList $ map (\v -> (v, [v])) initVertices
    continueGraph initVertices outputs reachable $ size - length initVertices
  where
    some' = bool 4 1 isBamboo
    continueGraph
        :: [Tx]
        -> [(Tx, Int)]
        -> HM.HashMap Tx [Tx]
        -> Int
        -> Gen ([Tx], HM.HashMap Tx [Tx])
    continueGraph vertices _ reachable 0 = pure (vertices, reachable)
    continueGraph vertices unusedUtxo reachable k
      | null unusedUtxo = pure (vertices, reachable)
      | otherwise =  do
        -- how many nodes to connect to (how many utxo to use)
        depsN <-
            max 1 . min (bool (min 3 $ length unusedUtxo) 1 isBamboo) <$> arbitrary
        chosenUtxo <- NE.fromList <$> sublistN depsN unusedUtxo
        -- grab some inputs
        let inputs = map (\(h,i) -> TxIn (hash h) (fromIntegral i)) chosenUtxo
        (Positive outputsN) <- resize some' arbitrary
        -- gen some outputs
        outputs <- NE.fromList <$> replicateM outputsN (TxOut <$> arbitrary <*> arbitrary)
        -- calculate new utxo & add vertex
        let tx = UnsafeTx inputs outputs $ mkAttributes ()
            producedUtxo = map (tx,) $ [0..(length outputs) - 1]
            newVertices = tx : vertices
            newUtxo = (unusedUtxo \\ toList chosenUtxo) ++ producedUtxo
            newReachableV = tx : concat (mapMaybe (\(x,_) -> HM.lookup x reachable)
                                         $ toList chosenUtxo)
            newReachable = HM.insert tx newReachableV reachable
        continueGraph newVertices newUtxo newReachable (k - 1)

-- | Primitive transaction generator with restriction on
-- inputs/outputs size
txGen :: Int -> Gen Tx
txGen size = do
    (Positive inputsN) <- resize size arbitrary
    (Positive outputsN) <- resize size arbitrary
    inputs <- NE.fromList <$> (replicateM inputsN $ (\h -> TxIn h 0) <$> arbitrary)
    outputs <-
        NE.fromList <$> (replicateM outputsN $ TxOut <$> arbitrary <*> arbitrary)
    case mkTx inputs outputs (mkAttributes ()) of
        Left e   -> error $ "txGen: something went wrong: " <> e
        Right tx -> pure tx
