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
import qualified Data.Set              as S (Set, delete, deleteFindMin, fromList, size,
                                             toList)
import           Pos.Constants         (epochSlots)
import           Pos.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Types             (Address, Coin (..), FtsSeed, TxId, TxOut (..),
                                        Utxo)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, NonEmptyList (..), resize,
                                        sized, suchThat)
import           Universum

spec :: Spec
spec = do
    let smaller = modifyMaxSize (const 10) . modifyMaxSuccess (const 2)
    describe "FollowTheSatoshi" $ do
        describe "followTheSatoshi" $ do
            describe "deterministic" $ do
                prop description_ftsListLength ftsListLength
                prop description_ftsNoStake ftsNoStake
                prop description_ftsAllStake ftsAllStake
            describe "probabilistic" $ smaller $ do
                prop description_ftsReasonableStake ftsReasonableStake
  where
    description_ftsListLength =
        "the amount of stakeholders is the same as the number of slots in an epoch"
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
        adr1 <- arbitrary
        adr2 <- arbitrary `suchThat` ((/=) adr1)
        listAdr <- arbitrary
        txId <- arbitrary
        coins <- (arbitrary :: Gen Coin)
        let setAdr = S.fromList $ adr1 : adr2 : listAdr
            (myAdr, setUtxo) = S.deleteFindMin setAdr
            nAdr = S.size setUtxo
            values = replicate nAdr coins
            utxoList =
                (replicate nAdr txId `zip` [0 .. fromIntegral nAdr]) `zip`
                (zipWith (flip TxOut) values $ S.toList setUtxo)
        return (myAdr, M.fromList utxoList)

newtype FtsStream = Stream
    { getStream :: S.Set FtsSeed
    } deriving Show

instance Arbitrary FtsStream where
    arbitrary = Stream <$> (sized $ const $ resize 1001 arbitrary)

ftsListLength :: FtsSeed -> StakeAndHolder -> Bool
ftsListLength fts (getNoStake -> (_, utxo)) =
    (length $ followTheSatoshi fts utxo) == epochSlots

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
    in all (== txOutAddress) $ followTheSatoshi fts utxo

ftsReasonableStake
    :: FtsStream
    -> StakeAndHolder
    -> Bool
ftsReasonableStake (getStream -> ftsSet) (getNoStake -> (_, utxo)) =
    let res = go (0,0) ftsList
    in (abs $ res - stakeProbability) < 90.0
  where
    ftsList = S.toList ftsSet
    totalStake = fromIntegral $ sum $ map (getCoin . txOutValue) $ M.elems utxo
    TxOut adr (Coin coin) = snd $ M.findMin utxo
    stakeProbability = fromIntegral (coin * 100) / totalStake
    go :: (Int, Int) -> [FtsSeed] -> Double
    go (!p, !t) [] = fromIntegral (p * 100) / fromIntegral t
    go (!present, !total) (fts : next)
        | total < 1000 =
            if elem adr (followTheSatoshi fts utxo)
                then go (1+present, 1+total) next
                else go (present, 1+total) next
        | otherwise = fromIntegral (present * 100) / fromIntegral total
