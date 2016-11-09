{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.Map              as M (elems, findMin, fromList)
import qualified Data.Set              as S (delete, deleteFindMin, fromList, size,
                                             toList)
import           Pos.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Types             (Address, Coin (..), FtsSeed, TxId, TxOut (..),
                                        Utxo)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), NonEmptyList (..), infiniteListOf)
import           Universum

spec :: Spec
spec = describe "FollowTheSatoshi" $ do
    describe "followTheSatoshi" $ do
        prop description_ftsNoStake ftsNoStake
        prop description_ftsAllStake ftsAllStake
        prop description_ftsReasonableStake ftsReasonableStake
  where
    description_ftsNoStake =
        "a stakeholder with 0% stake won't ever be selected as slot leader"
    description_ftsAllStake =
        "a stakeholder with 100% stake will always be selected as slot leader, and he \
        \ will be the only stakeholder"
    description_ftsReasonableStake =
        "a stakeholder with n% stake will be chosen approximately n% of the time"

newtype StakeAndHolder = StakeAndHolder
    { getNoStake :: (Address, Utxo)
    } deriving Show

instance Arbitrary StakeAndHolder where
    arbitrary = StakeAndHolder <$> do
        l1 <- getNonEmpty <$> arbitrary
        l2 <- getNonEmpty <$> arbitrary
        txId <- arbitrary
        coins <- arbitrary
        let setAdr = S.fromList $ l1 ++ l2
            (newAdr, setUtxo) = S.deleteFindMin setAdr
            nAdr = S.size setUtxo
            values = replicate nAdr coins
            utxoList =
                (replicate nAdr txId `zip` [0 .. fromIntegral nAdr]) `zip`
                (zipWith (flip TxOut) values $ S.toList setUtxo)
        return (newAdr, M.fromList utxoList)

newtype FtsStream = Stream
    { getStream :: [FtsSeed]
    } deriving Show

instance Arbitrary FtsStream where
    arbitrary = Stream <$> infiniteListOf arbitrary

ftsNoStake
    :: FtsSeed
    -> StakeAndHolder
    -> Bool
ftsNoStake fts (getNoStake -> (txOutAddress, utxo)) =
    let nonEmpty = followTheSatoshi fts utxo
    in not $ elem txOutAddress nonEmpty

ftsAllStake
    :: FtsSeed
    -> ((TxId, Word32), TxOut)
    -> Bool
ftsAllStake fts (key, t@TxOut{..}) =
    let utxo = M.fromList [(key, t)]
        nonEmpty@(_ :| l) = followTheSatoshi fts utxo
    in (elem txOutAddress nonEmpty) &&
       (null $ S.delete txOutAddress $ S.fromList $ l)

ftsReasonableStake
    :: FtsStream
    -> StakeAndHolder
    -> Bool
ftsReasonableStake (getStream -> ftsList) (getNoStake -> (_, utxo)) =
    let res = go (0,0) ftsList
    in (abs $ res - stakeProbability) < 5.0
  where
    totalStake = fromIntegral $ sum $ map (getCoin . txOutValue) $ M.elems utxo
    TxOut adr (Coin coin) = snd $ M.findMin utxo
    stakeProbability = fromIntegral (coin * 100) / totalStake
    go :: (Int, Int) -> [FtsSeed] -> Double
    go (pres, 1000) _ = fromIntegral (pres * 100) / fromIntegral 1000
    go (!present, !total) (fts : next) =
        if elem adr (followTheSatoshi fts utxo)
            then go (1+present, 1+total) next
            else go (present, 1+total) next
