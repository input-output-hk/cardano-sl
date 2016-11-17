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
import           Pos.Types             (Address (..), BadSigsTx (..), GoodTx (..),
                                        OverflowTx (..), Tx (..), TxId, TxIn (..),
                                        TxOut (..), verifyTx, verifyTxAlone)
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
        prop description_badSigsTx badSigsTx
  where
    description_invalidateBadTx =
        "invalidates Txs with negative coins or empty inputs/outputs"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_overflowTx =
        "a well-formed transaction with input and output sums above maxBound :: Coin \
        \ is validated successfully"
    description_badSigsTx =
        "a transaction with inputs improperly signed is never validated"

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
    let inpSum = sum $ fmap (toInteger . txOutValue . snd) $ catMaybes extendedInputs
        outSum = sum $ fmap (toInteger . txOutValue) txOuts
    in inpSum >= outSum

individualTxPropertyVerifier :: GoodTx -> Bool
individualTxPropertyVerifier (getGoodTx -> ls) =
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

validateGoodTx :: GoodTx -> Bool
validateGoodTx g@(getGoodTx -> ls) =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        resolveFun (Tx _ txOut, tInp, _) = (tInp, head txOut)
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (map resolveFun ls)
        transactionIsVerified = isVerSuccess $ verifyTx inpResolver $ Tx{..}
        transactionReallyIsGood = individualTxPropertyVerifier g
    in  transactionIsVerified == transactionReallyIsGood

overflowTx :: OverflowTx -> Bool
overflowTx (getOverflowTx -> ls) =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        resolveFun (Tx _ txOut, tInp, _) = (tInp, head txOut)
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (map resolveFun ls)
        extendInput txIn = (txIn,) <$> inpResolver txIn
        extendedInputs :: [Maybe (TxIn, TxOut)]
        extendedInputs = fmap extendInput txInputs
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver Tx{..}
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
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        resolveFun (Tx _ txOut, tInp, _) = (tInp, head txOut)
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (map resolveFun ls)
        extendInput txIn = (txIn,) <$> inpResolver txIn
        extendedInputs :: [Maybe (TxIn, TxOut)]
        extendedInputs = fmap extendInput txInputs
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver Tx{..}
        noSignatureIsValid = all (signatureIsNotValid txOutputs) extendedInputs
    in noSignatureIsValid == transactionIsNotVerified
