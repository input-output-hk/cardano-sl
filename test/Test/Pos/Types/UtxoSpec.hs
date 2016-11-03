-- | Specification of Pos.Types.Utxo.

module Test.Pos.Types.UtxoSpec
       ( spec
       ) where

import           Pos.Crypto       (keyGen, sign, unsafeHash)
import           Pos.Types        (TxIn (..), findTxIn)
import           System.IO.Unsafe (unsafePerformIO)

import           Test.Hspec       (Spec, describe, it)
import           Universum

spec :: Spec
spec = describe "Types.Utxo" $ do
    describe "findTxIn" $ do
        it "returns Nothing when given empty list" $
            (findTxIn myTx mempty) == Nothing
  where
    myTx = (TxIn myHash 0 mySig)
    myHash = unsafeHash (0 :: Int)
    mySig = sign mySK (myHash, 0, [])
    mySK = unsafePerformIO $ snd <$> keyGen
