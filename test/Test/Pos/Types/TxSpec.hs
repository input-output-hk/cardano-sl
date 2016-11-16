{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Specification of Pos.Types.Tx.

module Test.Pos.Types.TxSpec
       ( spec
       ) where

import           Control.Lens          (view, _2, _3)
import           Control.Monad         (join)
import           Data.List             (lookup)
import           Pos.Crypto            (SecretKey, sign, toPublic, verify)
import           Pos.Types             (Address (..), GoodTx (..), Tx (..), TxId,
                                        TxIn (..), TxOut (..), verifyTx, verifyTxAlone)
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Types.Tx" $ do
    describe "verifyTxAlone" $ do
        prop description_invalidateBadTx invalidateBadTx
    describe "verifyTx" $ do
        prop description_validateGoodTx validateGoodTx
        prop description_overflowTx overflowTx
  where
    description_invalidateBadTx =
        "invalidates Txs with negative coins or empty inputs/outputs"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_overflowTx =
        "a well-formed transaction with input and output sums above maxBound :: Coin \
        \ is validated successfully"

invalidateBadTx :: Tx -> Bool
invalidateBadTx tx@Tx{..} =
    (isVerFailure $ mconcat $ fmap (verifyTxAlone . uncurry Tx) $
    [ ([], outputs) | outputs <- [[], negOutputs, txOutputs]] ++
    [ (inputs, outputs) | inputs <- [[], txInputs]
                        , outputs <- [[], negOutputs]]) &&
    (isVerSuccess $ verifyTxAlone tx)
  where
    negOutputs = fmap (\(TxOut a c) -> TxOut a (negate c)) txOutputs

txChecksum :: [Maybe (TxIn, TxOut)] -> [TxOut] -> Bool
txChecksum extendedInputs txOuts =
    let inpSum = sum $ fmap (txOutValue . snd) $ catMaybes extendedInputs
        outSum = sum $ fmap txOutValue txOuts
    in inpSum >= outSum

individualPropertyVerifier :: GoodTx -> Bool
individualPropertyVerifier (getGoodTx -> ls) =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (fmap (\(Tx _ o, ti, _) -> (ti, head o)) ls)
        extendInput txIn = (txIn,) <$> inpResolver txIn
        extendedInputs :: [Maybe (TxIn, TxOut)]
        extendedInputs = fmap extendInput txInputs
        hasGoodSum = txChecksum extendedInputs txOutputs
        hasGoodStructure = isVerSuccess $ verifyTxAlone Tx{..}
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

-- | Operator for logical equivalence
(<=>) :: Bool -> Bool -> Bool
b <=> p = ((not b) || p) && (b || (not p))

validateGoodTx :: GoodTx -> Bool
validateGoodTx g@(getGoodTx -> ls) =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        resolveFun (Tx _ txOut, tInp, _) = (tInp, head txOut)
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (map resolveFun ls)
        transactionIsVerified = isVerSuccess $ verifyTx inpResolver $ Tx{..}
        transactionReallyIsGood = individualPropertyVerifier g
    in  transactionIsVerified <=> transactionReallyIsGood

overflowTx :: TxId -> SecretKey -> Address -> Bool
overflowTx txInHash senderSk receiverAdr =
    let senderAdr = Address $ toPublic senderSk
        txOut1 = TxOut senderAdr maxBound -- note that maxBound + 1 == 0
        txOut2 = TxOut senderAdr 1
        txOut = [TxOut receiverAdr 100000]
        txsig1 = sign senderSk (txInHash, 0, txOut)
        txsig2 = sign senderSk (txInHash, 1, txOut)
        txIn1 = TxIn txInHash 0 txsig1
        txIn2 = TxIn txInHash 1 txsig2
        inpRes txInp =
            if txInp == txIn1
                then Just txOut1
                else Just txOut2
        tx = Tx [txIn1, txIn2] txOut
        sumOverflow = (txChecksum [Just (txIn1, txOut1), Just (txIn2, txOut2)] txOut)
        transactionIsVerified = isVerSuccess $ verifyTx inpRes tx
    in sumOverflow <=> transactionIsVerified
