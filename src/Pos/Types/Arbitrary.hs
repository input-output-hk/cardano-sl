{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | `Arbitrary` instances for core types for using in tests and benchmarks

module Pos.Types.Arbitrary
       ( GoodTx (..)
       ) where

import           Control.Lens               (view, _3)
import qualified Data.ByteString            as BS (pack)
import           Data.DeriveTH              (derive, makeArbitrary)
import           Data.Time.Units            (Microsecond, fromMicroseconds)
import           Pos.Constants              (epochSlots, ftsSeedLength)
import           Pos.Crypto                 (SecretKey, hash, sign, toPublic)
import           Pos.Types.Timestamp        (Timestamp (..))
import           Pos.Types.Types            (Address (..), ChainDifficulty (..),
                                             Coin (..), EpochIndex (..), FtsSeed (..),
                                             LocalSlotIndex (..), SlotId (..), Tx (..),
                                             TxIn (..), TxOut (..))
import           System.Random              (Random)
import           Test.QuickCheck            (Arbitrary (..), Gen, NonEmptyList (..),
                                             NonZero (..), choose, vector)
import           Universum

import           Pos.Crypto.Arbitrary       ()
import           Pos.Types.Arbitrary.Unsafe ()

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

instance Arbitrary Tx where
    arbitrary = do
        txIns <- getNonEmpty <$> arbitrary
        txOuts <- getNonEmpty <$> arbitrary
        return $ Tx txIns txOuts

newtype GoodTx = GoodTx
    { getGoodTx :: [(Tx, TxIn, TxOut)]
    } deriving (Show)

instance Arbitrary GoodTx where
    arbitrary = GoodTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        let fun (Tx txIn txOut, fromSk, toSk, c) =
                (Tx txIn $ (txO fromSk c) : txOut, fromSk, txO toSk c)
            txList = fmap fun txsList
            thisTxOutputs = fmap (view _3) txList
            newTx (tx, fromSk, txOutput) =
                let txHash = hash $ tx
                    txIn = TxIn txHash 0 (sign fromSk (txHash, 0, thisTxOutputs))
                in (tx, txIn, txOutput)
            txO s c = TxOut (Address $ toPublic s) c
            goodTx = fmap newTx txList
        return goodTx

instance Arbitrary FtsSeed where
    arbitrary = do
        bs <- vector ftsSeedLength
        return $ FtsSeed $ BS.pack bs

----------------------------------------------------------------------------
-- Arbitrary miscellaneous types
----------------------------------------------------------------------------

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)

deriving instance Arbitrary Timestamp
