{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Types.Tx.

module Test.Pos.Types.TxSpec
       ( spec
       ) where

import           Control.Lens          (view, _2, _3)
import           Control.Monad         (join)
import           Data.List             (lookup)
import           Pos.Types             (GoodTx (..), Tx (..), TxIn (..), TxOut (..),
                                        verifyTx, verifyTxAlone)
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Types.Tx" $ do
    describe "verifyTx" $ do
        prop description_invalidateBadTx invalidateBadTx
        prop description_validateGoodTx validateGoodTx
  where
    description_invalidateBadTx =
        "invalidates Txs with negative coins or empty inputs/outputs"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"

invalidateBadTx :: Tx -> Bool
invalidateBadTx tx@Tx{..} =
    (isVerFailure $ mconcat $ fmap (verifyTxAlone . uncurry Tx) $
    [ ([], outputs) | outputs <- [[], negOutputs, txOutputs]] ++
    [ (inputs, outputs) | inputs <- [[], txInputs]
                        , outputs <- [[], negOutputs]]) &&
    (isVerSuccess $ verifyTxAlone tx)
  where
    negOutputs = fmap (\(TxOut a c) -> TxOut a (negate c)) txOutputs

validateGoodTx :: GoodTx -> Bool
validateGoodTx (getGoodTx -> ls) =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (map (\(Tx _ o, ti, _) -> (ti, head o)) ls)
    in isVerSuccess $ verifyTx inpResolver $ Tx{..}
