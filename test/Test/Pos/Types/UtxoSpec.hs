-- | Specification of Pos.Types.Utxo.

module Test.Pos.Types.UtxoSpec
       ( spec
       ) where

import qualified Data.Map              as M (Map, delete, elems, fromList, insert, keys)
import           Data.Maybe            (isJust, isNothing)
import           Pos.Crypto            (hash, keyGen, sign, unsafeHash)
import           Pos.Types             (Tx (..), TxIn (..), TxOut, Utxo, applyTxToUtxo,
                                        deleteTxIn, findTxIn, verifyTxUtxo)
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)
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
  where
    myTx = (TxIn myHash 0 mySig)
    myHash = unsafeHash (0 :: Int)
    mySig = sign mySK (myHash, 0, [])
    mySK = unsafePerformIO $ snd <$> keyGen
    description_findTxInUtxo =
        "correctly finds the TxOut corresponding to (txHash, txIndex) when the key " ++
        "is in the Utxo map, and doesn't find it otherwise"
    description_deleteTxInUtxo =
        "deleting a (txHash, txIndex) key from a Utxo map where it is present " ++
        "returns the map without that key, and if it's not present it does nothing"

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

{-verifyTxInUtxo :: Tx -> Utxo -> Bool
verifyTxInUtxo Tx{..} utxo =
    let utxo' = foldr (flip M.delete utxo) $
            map (\TxIn th ti _ -> (th, ti)) txInputs
        key = (txInHash, txInIndex)
        utxo' = M.delete key utxo
        fun Tx{..} utxo = M.insert (txInHash, txInIndex) utxo
        newUtxo = foldr fun txInputs
    in (isVerSuccess == verifyTxUtxo newUtxo tx) &&
       (isVerFailure == verifyTxUtxo utxo' tx)-}

applyTxToUtxoGood :: M.Map TxIn TxOut -> [TxOut] -> Bool
applyTxToUtxoGood txMap txOuts =
    let txInps = M.keys txMap
        hashTx = hash $ Tx txInps txOuts
        inpFun = (\(TxIn h i _) -> (h,i))
        inpList = map inpFun txInps
        utxoMap = M.fromList $ zip inpList (M.elems txMap)
        newUtxoMap = applyTxToUtxo (Tx txInps txOuts) utxoMap
        newUtxos = ((repeat hashTx) `zip` [0 ..]) `zip` txOuts
        rmvUtxo = foldr M.delete utxoMap inpList
        insNewUtxo = foldr (uncurry M.insert) rmvUtxo newUtxos
    in insNewUtxo == newUtxoMap
