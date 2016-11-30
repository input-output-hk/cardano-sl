{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | `Arbitrary` instances for core types for using in tests and benchmarks

module Pos.Types.Arbitrary
       ( BadSigsTx (..)
       , GoodTx (..)
       , OverflowTx (..)
       , SmallBadSigsTx (..)
       , SmallHashMap (..)
       , SmallGoodTx (..)
       , SmallOverflowTx (..)
       ) where

import           Control.Lens               (over, view, _2, _3)
import qualified Data.ByteString            as BS (pack)
import           Data.DeriveTH              (derive, makeArbitrary)
import           Data.Time.Units            (Microsecond, fromMicroseconds)
import           Pos.Constants              (epochSlots, sharedSeedLength)
import           Pos.Crypto                 (LShare, PublicKey, SecretKey, hash, sign, toPublic)
import           Pos.Types.Timestamp        (Timestamp (..))
import           Pos.Types.Types            (Address (..), ChainDifficulty (..),
                                             Coin (..), EpochIndex (..),
                                             LocalSlotIndex (..), SharedSeed (..),
                                             SlotId (..), Tx (..), TxIn (..), TxOut (..))
import           System.Random              (Random)
import           Test.QuickCheck            (Arbitrary (..), Gen, NonEmptyList (..),
                                             NonZero (..), choose, scale, vector)
import           Test.QuickCheck.Instances  ()
import           Universum

import           Pos.Crypto.Arbitrary       ()
import           Pos.Types.Arbitrary.Unsafe ()

makeSmall :: Gen a -> Gen a
makeSmall = scale f
  where
    f 0 = 0
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 3
    f n
      | n < 0 = n
      | otherwise =
          (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3)) n

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

deriving instance Arbitrary Address
deriving instance Arbitrary ChainDifficulty

derive makeArbitrary ''SlotId
derive makeArbitrary ''TxOut

instance Arbitrary Coin where
    arbitrary = Coin . getNonZero <$> (arbitrary :: Gen (NonZero Word64))

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random EpochIndex

instance Arbitrary EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)

deriving instance Random LocalSlotIndex

instance Arbitrary LocalSlotIndex where
    arbitrary = choose (0, epochSlots - 1)

instance Arbitrary TxIn where
    arbitrary = do
        txId <- arbitrary
        txIdx <- arbitrary
        sk <- arbitrary
        let signature = sign sk (txId, txIdx, [])
        return $ TxIn txId txIdx signature

-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'verifyTxAlone'

instance Arbitrary Tx where
    arbitrary = do
        txIns <- getNonEmpty <$> arbitrary
        txOuts <- getNonEmpty <$> arbitrary
        return $ Tx txIns txOuts

-- | Type used to generate valid (w.r.t 'verifyTxAlone' and 'verifyTx')
-- transactions and accompanying input information.
-- It's not entirely general because it only generates transactions whose
-- outputs are in the same number as its inputs in a one-to-one correspondence.
--
-- The GoodTx type is a list of triples where the third elements are the
-- transaction's outputs, the second elements are its inputs, and the first are
-- the transactions from where the tuple's TxIn came from.
--
-- The OverflowTx type is the same as GoodTx, except its values, both for
-- inputs as well as outputs, are very close to maxBound :: Coin so as to cause
-- overflow in the Coin type if they are summed.
--
-- The BadSigTx type is also the same as GoodTx, with the difference that all
-- signatures in the transaction's inputs have been replaced with a bogus one.

buildProperTx
    :: [(Tx, SecretKey, SecretKey, Coin)]
    -> (Coin -> Coin, Coin -> Coin)
    -> Gen [(Tx, TxIn, TxOut)]
buildProperTx triplesList (inCoin, outCoin)= do
        let fun (Tx txIn txOut, fromSk, toSk, c) =
                let inC = inCoin c
                    outC = outCoin c
                    txToBeSpent = Tx txIn $ (makeTxOutput fromSk inC) : txOut
                in (txToBeSpent, fromSk, makeTxOutput toSk outC)
            txList = fmap fun triplesList
            thisTxOutputs = fmap (view _3) txList
            newTx (tx, fromSk, txOutput) =
                let txHash = hash tx
                    txIn = TxIn txHash 0 (sign fromSk (txHash, 0, thisTxOutputs))
                in (tx, txIn, txOutput)
            makeTxOutput s c = TxOut (Address $ toPublic s) c
            goodTx = fmap newTx txList
        return goodTx

-- | Well-formed transaction 'Tx'.
newtype GoodTx = GoodTx
    { getGoodTx :: [(Tx, TxIn, TxOut)]
    } deriving (Show)

newtype SmallGoodTx =
    SmallGoodTx GoodTx
    deriving Show

instance Arbitrary GoodTx where
    arbitrary = GoodTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        buildProperTx txsList (identity, identity)

instance Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with overflow.
newtype OverflowTx = OverflowTx
    { getOverflowTx :: [(Tx, TxIn, TxOut)]
    } deriving (Show)

newtype SmallOverflowTx =
    SmallOverflowTx OverflowTx
    deriving Show

instance Arbitrary OverflowTx where
    arbitrary = OverflowTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        let halfBound = maxBound `div` 2
        buildProperTx txsList ((halfBound +), (halfBound -))

instance Arbitrary SmallOverflowTx where
    arbitrary = SmallOverflowTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: [(Tx, TxIn, TxOut)]
    } deriving (Show)

newtype SmallBadSigsTx =
    SmallBadSigsTx BadSigsTx
    deriving Show

instance Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        let addBadSig t = t {txInSig = badSig}
        return $ fmap (over _2 addBadSig) goodTxList

instance Arbitrary SmallBadSigsTx where
    arbitrary = SmallBadSigsTx <$> makeSmall arbitrary

instance Arbitrary SharedSeed where
    arbitrary = do
        bs <- vector sharedSeedLength
        return $ SharedSeed $ BS.pack bs

----------------------------------------------------------------------------
-- Arbitrary miscellaneous types
----------------------------------------------------------------------------

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)

deriving instance Arbitrary Timestamp

newtype SmallHashMap =
    SmallHashMap (HashMap PublicKey (HashMap PublicKey LShare))
    deriving Show

instance Arbitrary SmallHashMap where
    arbitrary = SmallHashMap <$> makeSmall arbitrary
