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
import           Data.List             (lookup)
import           Data.List             ((\\))
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)
import           Test.Hspec            (Spec, describe, it, pendingWith)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..), Positive (..), arbitrary,
                                        forAll, resize, shuffle, sized, vectorOf, (.&.),
                                        (===))
import           Test.QuickCheck.Gen   (Gen)
import           Universum

import           Pos.Crypto            (hash, verify)
import           Pos.Types             (Address (..), BadSigsTx (..), GoodTx (..),
                                        OverflowTx (..), Tx (..), TxIn (..), TxOut (..),
                                        topsortTxs, verifyTx, verifyTxAlone)
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
        it "does correct topsort for a acyclic graph" $
            pendingWith "not implemented"
--            forAll (txAcyclicGen 15) $ \txs ->
--            forAll (shuffle txs) $ \shuffled ->
--            undefined
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
    description_badSigsTx =
        "a transaction with inputs improperly signed is never validated"

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
        mapFun =
            \maybeTxPair ->
                case maybeTxPair of
                    Nothing -> False
                    Just (TxIn{..}, TxOut{..}) ->
                        verify (getAddress txOutAddress)
                               (txInHash, txInIndex, txOutputs)
                               txInSig
        hasGoodInputs = and $ map mapFun extendedInputs
    in hasGoodSum && hasGoodStructure && hasGoodInputs

validateGoodTx :: GoodTx -> Bool
validateGoodTx (getGoodTx -> ls) =
    let triple@(tx, inpResolver, _) =
            getTxFromGoodTx ls
        transactionIsVerified = isVerSuccess $ verifyTx inpResolver tx
        transactionReallyIsGood = individualTxPropertyVerifier triple
    in  transactionIsVerified == transactionReallyIsGood

overflowTx :: OverflowTx -> Bool
overflowTx (getOverflowTx -> ls) =
    let (tx@Tx{..}, inpResolver, extendedInputs) =
            getTxFromGoodTx ls
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver tx
        inpSumLessThanOutSum = not $ txChecksum extendedInputs txOutputs
    in inpSumLessThanOutSum == transactionIsNotVerified

signatureIsNotValid :: [TxOut] -> Maybe (TxIn, TxOut) -> Bool
signatureIsNotValid txOutputs (Just (TxIn{..}, TxOut{..})) =
    not $ verify (getAddress txOutAddress)
        (txInHash, txInIndex, txOutputs)
        txInSig
signatureIsNotValid _ _ = False

badSigsTx :: BadSigsTx -> Bool
badSigsTx (getBadSigsTx -> ls) =
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
    inputs <- replicateM inputsN $ (\h s -> TxIn h 0 s) <$> arbitrary <*> arbitrary
    outputs <- replicateM outputsN $
        (\p (Positive c) -> TxOut (Address p) c) <$> arbitrary <*> (resize 100 arbitrary)
    pure $ Tx inputs outputs

-- | Produces acyclic oriented graph of transactions. It's
-- connected. Signatures are faked and thus fail to
-- verify. Transaction balance is bad too (input can be less than
-- output). These properties are not needed for topsort test. It also
-- returns reachability map as the second argument (for every key
-- elems are reachable txs).
txAcyclicGen :: Int -> Gen [Tx]
txAcyclicGen 0 = pure []
txAcyclicGen size = do
    initVertex <- txGen 10
    let outputs = (\tx -> map (hash tx,) [0..length (txOutputs tx) - 1]) initVertex
    continueGraph [initVertex] outputs $ size - 1
  where
    continueGraph vertices _ 0   = pure vertices
    continueGraph vertices unusedUtxo k = do
        -- how many nodes to connect to (how many utxo to use)
        (NonNegative depsN) <-
            resize (max (length unusedUtxo) 3)
                   (arbitrary :: Gen (NonNegative Int))
        chosenUtxo <- sublistN depsN unusedUtxo
        -- grab some inputs
        inputs <- mapM (\(h,i) -> TxIn h (fromIntegral i) <$> arbitrary) chosenUtxo
        (Positive outputsN) <- resize 4 arbitrary
        -- gen some outputs
        outputs <- replicateM outputsN $
            (\p (Positive c) -> TxOut (Address p) c) <$>
            arbitrary <*>
            (resize 100 arbitrary)
        -- calculate new utxo & add vertex
        let tx = Tx inputs outputs
            producedUtxo = map (hash tx,) $ [0..(length outputs) - 1]
            newVertices = tx : vertices
            newUtxo = (unusedUtxo \\ chosenUtxo) ++ producedUtxo
        continueGraph newVertices newUtxo (k-1)
