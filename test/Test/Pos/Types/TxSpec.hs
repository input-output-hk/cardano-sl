{-# LANGUAGE ViewPatterns #-}

-- | Specification of Pos.Types.Tx.

module Test.Pos.Types.TxSpec
       ( spec
       ) where

import           Control.Lens          (view, _3)
import           Data.List             (lookup)
import           Pos.Crypto            (SecretKey, sign, toPublic)
import           Pos.Types             (Address (..), Coin, Tx (..), TxId (..), TxIn (..),
                                        TxOut (..), verifyTx, verifyTxAlone)
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, NonEmptyList (..))
import           Universum

newtype GoodTx = GoodTx
    { getGoodTx ::[(TxIn, TxOut)]
    } deriving (Show)

instance Arbitrary GoodTx where
    arbitrary = GoodTx <$> do
        list <- getNonEmpty <$> arbitrary-- :: Gen (NonEmptyList (TxId, Word32, SecretKey, Coin))
        let txIns = fmap fun list
            fun (tid, ix, sk, c) = (TxIn tid ix $ sign sk (tid, ix, [txOut sk c]), txOut sk c)
            txOut s c = TxOut (Address $ toPublic s) c
        return txIns

spec :: Spec
spec = describe "Types.Tx" $ do
    describe "verifyTx" $ do
        prop description_invalidateBadTx invalidateBadTx
        --prop description_validateGoodTxs verifyGoodTx
  where
    description_invalidateBadTx =
        "invalidates Txs with negative coins or empty inputs/outputs"
    {-description_validateGoodTxs =
        "validates transaction whose inputs are proper outputs, and " ++
         "whose outputs are correct as well"-}

invalidateBadTx :: Tx -> Bool
invalidateBadTx tx@Tx{..} =
    (isVerFailure $ mconcat $ fmap (verifyTxAlone . uncurry Tx) $
    [ ([], outputs) | outputs <- [[], negOutputs, txOutputs]] ++
    [ (inputs, outputs) | inputs <- [[], txInputs]
                        , outputs <- [[], negOutputs]]) &&
    (isVerSuccess $ verifyTxAlone tx)
  where
    negOutputs = fmap (\(TxOut a c) -> TxOut a (negate c)) txOutputs

{-verifyGoodTx :: GoodTx -> Bool
verifyGoodTx (getGoodTx -> list) =
    let txouts = fmap snd list
        txinps = fmap fst list
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = flip lookup list
    in isVerSuccess $ verifyTx inpResolver $ Tx txinps txouts
-}
