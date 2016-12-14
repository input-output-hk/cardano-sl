{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Control.Lens               (set, view, _3, _4)
import qualified Data.ByteString            as BS (pack)
import           Data.DeriveTH              (derive, makeArbitrary)
import           Data.Time.Units            (Microsecond, fromMicroseconds)
import           Pos.Constants              (epochSlots, sharedSeedLength)
import           Pos.Crypto                 (Share, PublicKey, SecretKey, hash, sign,
                                             toPublic)
import           Pos.Types.Timestamp        (Timestamp (..))
import           Pos.Types.Types            (Address (..), ChainDifficulty (..),
                                             Coin (..), EpochIndex (..),
                                             LocalSlotIndex (..), SharedSeed (..),
                                             SlotId (..), Tx (..), TxIn (..),
                                             TxInWitness (..), TxOut (..),

                                             makePubKeyAddress, makeScriptAddress)
import           Pos.Util                   (AsBinary)
import           System.Random              (Random)
import           Test.QuickCheck            (Arbitrary (..), Gen, NonEmptyList (..),
                                             NonZero (..), choose, elements, oneof, scale,
                                             vector)
import           Test.QuickCheck.Instances  ()
import           Universum

import           Pos.Binary.Class           (Bi)
import           Pos.Crypto.Arbitrary       ()
import           Pos.Script                 (Script, parseRedeemer, parseValidator)
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
-- Validator and redeemer scripts
----------------------------------------------------------------------------

tfValidator :: Script
Right tfValidator = parseValidator $ unlines [
    "data Bool = { True | False }",
    "validator : (forall a . a -> a -> a) -> Comp Bool {",
    "  validator f = case f True False of {",
    "    True  -> success True : Comp Bool;",
    "    False -> failure : Comp Bool } }" ]

goodTfRedeemer :: Script
Right goodTfRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp (forall a . a -> a -> a) {",
    "  redeemer = success (\\t f -> t) }" ]

badTfRedeemer :: Script
Right badTfRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp (forall a . a -> a -> a) {",
    "  redeemer = success (\\t f -> f) }" ]

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

instance Arbitrary Script where
    arbitrary = elements
        [tfValidator, goodTfRedeemer, badTfRedeemer]

instance Arbitrary Address where
    arbitrary = oneof [
        makePubKeyAddress <$> arbitrary,
        makeScriptAddress <$> arbitrary ]

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

instance (Bi TxOut, Bi Tx) => Arbitrary TxInWitness where
    arbitrary = oneof [
        PkWitness <$> arbitrary <*> arbitrary,
        -- this can generate a redeemer script where a validator script is
        -- needed and vice-versa, but it doesn't matter
        ScriptWitness <$> arbitrary <*> arbitrary ]

instance Bi Tx => Arbitrary TxIn where
    arbitrary = do
        txId <- arbitrary
        txIdx <- arbitrary
        return (TxIn txId txIdx)

-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'verifyTxAlone'

instance Bi Tx => Arbitrary Tx where
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
    :: (Bi Tx, Bi TxOut)
    => [(Tx, SecretKey, SecretKey, Coin)]
    -> (Coin -> Coin, Coin -> Coin)
    -> Gen [(Tx, TxIn, TxOut, TxInWitness)]
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
                    txIn = TxIn txHash 0
                    witness = PkWitness {
                        twKey = toPublic fromSk,
                        twSig = sign fromSk (txHash, 0, thisTxOutputs) }
                in (tx, txIn, txOutput, witness)
            makeTxOutput s c = TxOut (makePubKeyAddress $ toPublic s) c
            goodTx = fmap newTx txList
        return goodTx

-- | Well-formed transaction 'Tx'.
newtype GoodTx = GoodTx
    { getGoodTx :: [(Tx, TxIn, TxOut, TxInWitness)]
    } deriving (Show)

newtype SmallGoodTx =
    SmallGoodTx GoodTx
    deriving Show

instance (Bi Tx, Bi TxOut) => Arbitrary GoodTx where
    arbitrary = GoodTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        buildProperTx txsList (identity, identity)

instance (Bi Tx, Bi TxOut) => Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with overflow.
newtype OverflowTx = OverflowTx
    { getOverflowTx :: [(Tx, TxIn, TxOut, TxInWitness)]
    } deriving (Show)

newtype SmallOverflowTx =
    SmallOverflowTx OverflowTx
    deriving Show

instance (Bi Tx, Bi TxOut) => Arbitrary OverflowTx where
    arbitrary = OverflowTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        let halfBound = maxBound `div` 2
        buildProperTx txsList ((halfBound +), (halfBound -))

instance (Bi Tx, Bi TxOut) => Arbitrary SmallOverflowTx where
    arbitrary = SmallOverflowTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: [(Tx, TxIn, TxOut, TxInWitness)]
    } deriving (Show)

newtype SmallBadSigsTx =
    SmallBadSigsTx BadSigsTx
    deriving Show

instance (Bi Tx, Bi TxOut) => Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList

instance (Bi Tx, Bi TxOut) => Arbitrary SmallBadSigsTx where
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
    SmallHashMap (HashMap PublicKey (HashMap PublicKey (AsBinary Share)))
    deriving Show

instance Arbitrary SmallHashMap where
    arbitrary = SmallHashMap <$> makeSmall arbitrary
