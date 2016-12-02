{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Specification for transaction-related functions
-- (Pos.Types.Tx)
module Test.Pos.Types.TxSpec
       ( spec
       ) where

import           Control.Lens          (view, _2, _3)
import           Control.Monad         (join)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (elemIndex, lookup, (\\))
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..), Positive (..), Property,
                                        arbitrary, forAll, resize, shuffle, vectorOf,
                                        (.&.), (===))
import           Test.QuickCheck.Gen   (Gen)
import           Universum             hiding ((.&.))

import           Pos.Crypto            (checkSig, hash)
import           Pos.Types             (BadSigsTx (..), GoodTx (..), OverflowTx (..),
                                        Redeemer (..), SmallBadSigsTx (..),
                                        SmallGoodTx (..), SmallOverflowTx (..), Tx (..),
                                        TxIn (..), TxOut (..), Validator (..),
                                        checkPubKeyAddress, topsortTxs, verifyTx,
                                        verifyTxAlone)
import           Pos.Util              (sublistN)


spec :: Spec
spec = describe "Types.Tx" $ do
    describe "verifyTxAlone" $ do
        prop description_validateGoodTxAlone validateGoodTxAlone
        prop description_invalidateBadTxAlone invalidateBadTxAlone
    describe "verifyTx" $ do
        prop description_validateGoodTx validateGoodTx
        prop description_overflowTx overflowTx
        prop description_badSigsTx badSigsTx
    describe "topsortTxs" $ do
        prop "doesn't change the random set of transactions" $
            forAll (resize 10 $ arbitrary) $ \(NonNegative l) ->
            forAll (vectorOf l (txGen 10)) $ \txs ->
            (sort <$> topsortTxs txs) === Just (sort txs)
        prop "graph generator does not produce loops" $
            forAll (txAcyclicGen False 20) $ \(txs,_) ->
            forAll (shuffle txs) $ \shuffled ->
            isJust $ topsortTxs shuffled
        prop "does correct topsort on bamboo" $ testTopsort True
        prop "does correct topsort on arbitrary acyclic graph" $ testTopsort False
  where
    description_validateGoodTxAlone =
        "validates Txs with positive coins and non-empty inputs and outputs"
    description_invalidateBadTxAlone =
        "invalidates Txs with non-positive coins or empty inputs/outputs"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_overflowTx =
        "a well-formed transaction with input and output sums above maxBound :: Coin \
        \is validated successfully"
    description_badSigsTx = "a transaction with inputs improperly signed is never validated"

validateGoodTxAlone :: Tx -> Bool
validateGoodTxAlone tx = isVerSuccess $ verifyTxAlone tx

invalidateBadTxAlone :: Tx -> Bool
invalidateBadTxAlone Tx {..} = all (isVerFailure . verifyTxAlone) badTxs
  where
    zeroOutputs = fmap (\(TxOut a _) -> TxOut a (negate 0)) txOutputs
    badTxs =
        map (uncurry Tx) $
        [([], txOutputs), (txInputs, []), (txInputs, zeroOutputs)]

type TxVerifyingTools = (Tx, TxIn -> Maybe TxOut, [Maybe (TxIn, TxOut)])

-- | This function takes the list inside a 'GoodTx' and related types, and
-- turns it into something 'verifyTx' can use:
--
-- * the transaction that the list holds
-- * the input resolver associated with that transaction
-- * the list of resolved inputs with all inputs in the transaction
getTxFromGoodTx :: [(Tx, TxIn, TxOut)] -> TxVerifyingTools
getTxFromGoodTx ls =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (fmap (\(Tx _ o, ti, _) -> (ti, head o)) ls)
        extendInput txIn = (txIn,) <$> inpResolver txIn
        extendedInputs :: [Maybe (TxIn, TxOut)]
        extendedInputs = fmap extendInput txInputs
    in (Tx {..}, inpResolver, extendedInputs)

-- | This function takes a list of resolved inputs from a transaction, that
-- same transaction's outputs, and verifies that the input sum is greater than
-- the output sum.
txChecksum :: [Maybe (TxIn, TxOut)] -> [TxOut] -> Bool
txChecksum extendedInputs txOuts =
    let inpSum = sum $ fmap (toInteger . txOutValue . snd) $ catMaybes extendedInputs
        outSum = sum $ fmap (toInteger . txOutValue) txOuts
    in inpSum >= outSum

-- | This function, used in 'verifyGoodTx', takes a 'GoodTx' and checks that
-- each property verified by 'verifyTx' holds, meaning:
--
-- * sum of inputs ≥ sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
-- It also checks that it has good structure w.r.t. 'verifyTxAlone'.
individualTxPropertyVerifier :: TxVerifyingTools -> Bool
individualTxPropertyVerifier (tx@Tx{..}, _, extendedInputs) =
    let hasGoodSum = txChecksum extendedInputs txOutputs
        hasGoodStructure = isVerSuccess $ verifyTxAlone tx
        hasGoodInputs = and $ map (signatureIsValid txOutputs) extendedInputs
    in hasGoodSum && hasGoodStructure && hasGoodInputs

validateGoodTx :: SmallGoodTx -> Bool
validateGoodTx (SmallGoodTx (getGoodTx -> ls)) =
    let triple@(tx, inpResolver, _) =
            getTxFromGoodTx ls
        transactionIsVerified = isVerSuccess $ verifyTx inpResolver tx
        transactionReallyIsGood = individualTxPropertyVerifier triple
    in  transactionIsVerified == transactionReallyIsGood

overflowTx :: SmallOverflowTx -> Bool
overflowTx (SmallOverflowTx (getOverflowTx -> ls)) =
    let (tx@Tx{..}, inpResolver, extendedInputs) =
            getTxFromGoodTx ls
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver tx
        inpSumLessThanOutSum = not $ txChecksum extendedInputs txOutputs
    in inpSumLessThanOutSum == transactionIsNotVerified

signatureIsValid :: [TxOut] -> Maybe (TxIn, TxOut) -> Bool
signatureIsValid txOutputs (Just (TxIn{..}, TxOut{..})) =
    let pk = getValidator txInValidator
        sig = getRedeemer txInRedeemer
    in checkPubKeyAddress pk txOutAddress &&
       checkSig pk (txInHash, txInIndex, txOutputs) sig
signatureIsValid _ _ = False

signatureIsNotValid :: [TxOut] -> Maybe (TxIn, TxOut) -> Bool
signatureIsNotValid txOutputs = not . signatureIsValid txOutputs

badSigsTx :: SmallBadSigsTx -> Bool
badSigsTx (SmallBadSigsTx (getBadSigsTx -> ls)) =
    let (tx@Tx{..}, inpResolver, extendedInputs) =
            getTxFromGoodTx ls
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver tx
        notAllSignaturesAreValid = any (signatureIsNotValid txOutputs) extendedInputs
    in notAllSignaturesAreValid == transactionIsNotVerified

-- | Primitive transaction generator with restriction on
-- inputs/outputs size
txGen :: Int -> Gen Tx
txGen size = do
    (Positive inputsN) <- resize size arbitrary
    (Positive outputsN) <- resize size arbitrary
    inputs <- replicateM inputsN $ (\h v r -> TxIn h 0 v r) <$>
              arbitrary <*> arbitrary <*> arbitrary
    outputs <- replicateM outputsN $
        (\addr (Positive c) -> TxOut addr c) <$> arbitrary <*> arbitrary
    pure $ Tx inputs outputs

testTopsort :: Bool -> Property
testTopsort isBamboo =
    forAll (txAcyclicGen isBamboo 40) $ \(txs,reach) ->
    forAll (shuffle txs) $ \shuffled ->
    let reachables :: [(Tx,Tx)]
        reachables = [(from,to) | (to,froms) <- HM.toList reach, from <- froms]
        topsorted = topsortTxs shuffled
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
            concatMap (\tx -> map (tx,) [0..length (txOutputs tx) - 1])
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
    continueGraph vertices unusedUtxo reachable k = do
        -- how many nodes to connect to (how many utxo to use)
        (NonNegative depsN) <-
            resize (bool (min 3 $ length unusedUtxo) 1 isBamboo) arbitrary
        chosenUtxo <- sublistN depsN unusedUtxo
        -- grab some inputs
        inputs <- mapM (\(h,i) -> TxIn (hash h) (fromIntegral i) <$> arbitrary <*> arbitrary) chosenUtxo
        (Positive outputsN) <- resize some' arbitrary
        -- gen some outputs
        outputs <- replicateM outputsN $
            (\addr (Positive c) -> TxOut addr c) <$> arbitrary <*> arbitrary
        -- calculate new utxo & add vertex
        let tx = Tx inputs outputs
            producedUtxo = map (tx,) $ [0..(length outputs) - 1]
            newVertices = tx : vertices
            newUtxo = (unusedUtxo \\ chosenUtxo) ++ producedUtxo
            newReachableV = tx : concat (mapMaybe (\(x,_) -> HM.lookup x reachable) chosenUtxo)
            newReachable = HM.insert tx newReachableV reachable
        continueGraph newVertices newUtxo newReachable (k-1)
