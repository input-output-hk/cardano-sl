{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Types.Utxo.

module Test.Pos.Types.UtxoSpec
       ( spec
       ) where

import           Control.Lens          (view, _1)
import qualified Data.Map              as M (Map, delete, elems, fromList, insert, keys)
import           Data.Maybe            (isJust, isNothing)
import           Pos.Crypto            (hash, keyGen, sign, unsafeHash)
import           Pos.Types             (GoodTx (..), Redeemer (..), SmallGoodTx (..),
                                        Tx (..), TxIn (..), TxOut, Utxo, Validator (..),
                                        applyTxToUtxo, deleteTxIn, findTxIn, verifyTxUtxo)
import           Serokell.Util.Verify  (isVerSuccess)
import           System.IO.Unsafe      (unsafePerformIO)

import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Types.Utxo" $ do
    describe "findTxIn" $ do
        it "returns Nothing when given empty list" $
            (findTxIn myTx mempty) == Nothing
        prop description_findTxInUtxo findTxInUtxo
    describe "deleteTxIn" $ do
        prop description_deleteTxInUtxo deleteTxInUtxo
    describe "verifyTxUtxo" $ do
        prop description_verifyTxInUtxo verifyTxInUtxo
    describe "applyTxToUtxo" $ do
        prop description_applyTxToUtxoGood applyTxToUtxoGood
  where
    myTx = (TxIn myHash 0 myValidator myRedeemer)
    myHash = unsafeHash (0 :: Int)
    myValidator = PubKeyValidator myPK
    myRedeemer = PubKeyRedeemer $ sign mySK (myHash, 0, [])
    (myPK, mySK) = unsafePerformIO keyGen
    description_findTxInUtxo =
        "correctly finds the TxOut corresponding to (txHash, txIndex) when the key is in\
        \ the Utxo map, and doesn't find it otherwise"
    description_deleteTxInUtxo =
        "deleting a (txHash, txIndex) key from a Utxo map where it is present returns\
        \ the map without that key, and if it's not present it does nothing"
    description_applyTxToUtxoGood =
        "correctly removes spent outputs used as inputs in given transaction and\
        \ successfully adds this transaction's outputs to the utxo map"
    description_verifyTxInUtxo =
        "successfully verifies a transaction whose inputs are all present in the utxo\
        \ map"

findTxInUtxo :: TxIn -> TxOut -> Utxo -> Bool
findTxInUtxo t@TxIn{..} txO utxo =
    let key = (txInHash, txInIndex)
        utxo' = M.delete key utxo
        newUtxo = M.insert key txO utxo
    in (isJust $ findTxIn t newUtxo) && (isNothing $ findTxIn t utxo')

deleteTxInUtxo :: TxIn -> TxOut -> Utxo -> Bool
deleteTxInUtxo t@TxIn{..} txO utxo =
    let key = (txInHash, txInIndex)
        utxo' = M.delete key utxo
        newUtxo = M.insert key txO utxo
    in (utxo' == deleteTxIn t newUtxo) && (utxo' == deleteTxIn t utxo')

verifyTxInUtxo :: SmallGoodTx -> Bool
verifyTxInUtxo (SmallGoodTx (getGoodTx -> ls)) =
    let txs = fmap (view _1) ls
        newTx = uncurry Tx $ unzip $ map (\(_, tIs, tOs) -> (tIs, tOs)) ls
        utxo = foldr applyTxToUtxo mempty txs
    in isVerSuccess $ verifyTxUtxo utxo newTx

applyTxToUtxoGood :: M.Map TxIn TxOut -> [TxOut] -> Bool
applyTxToUtxoGood txMap txOuts =
    let txInps = M.keys txMap
        hashTx = hash $ Tx txInps txOuts
        inpFun = (\(TxIn h i _ _) -> (h,i))
        inpList = map inpFun txInps
        utxoMap = M.fromList $ zip inpList (M.elems txMap)
        newUtxoMap = applyTxToUtxo (Tx txInps txOuts) utxoMap
        newUtxos = ((repeat hashTx) `zip` [0 ..]) `zip` txOuts
        rmvUtxo = foldr M.delete utxoMap inpList
        insNewUtxo = foldr (uncurry M.insert) rmvUtxo newUtxos
    in insNewUtxo == newUtxoMap
