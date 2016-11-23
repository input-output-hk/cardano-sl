{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import qualified Data.Map              as M (elems, fromList, insert)
import qualified Data.Set              as S (deleteFindMin, fromList, size,
                                             toList)
import           Pos.Constants         (epochSlots)
import           Pos.Crypto            (unsafeHash)
import           Pos.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Types             (Address, Coin (..), SharedSeed, TxId, TxOut (..),
                                        Utxo)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, infiniteListOf, suchThat)
import           Universum

spec :: Spec
spec = do
    let smaller = modifyMaxSuccess (const 10)
    describe "FollowTheSatoshi" $ do
        describe "followTheSatoshi" $ do
            describe "deterministic" $ do
                prop description_ftsListLength ftsListLength
                prop description_ftsNoStake ftsNoStake
                prop description_ftsAllStake ftsAllStake
            describe "probabilistic" $ smaller $ do
                prop description_ftsReasonableStake
		    (ftsReasonableStake 0.02 $ 2 * fromIntegral epochSlots * 0.02)
		prop description_ftsReasonableStake (ftsReasonableStake 0.98 0.1)
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

-- | Type used to generate random Utxo and an address that is not in this map,
-- meaning it does not hold any stake in the system's current state.
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
    { getStream :: [SharedSeed]
    } deriving Show

instance Arbitrary FtsStream where
    arbitrary = Stream . take 100000 <$> infiniteListOf arbitrary

ftsListLength :: SharedSeed -> StakeAndHolder -> Bool
ftsListLength fts (getNoStake -> (_, utxo)) =
    (length $ followTheSatoshi fts utxo) == epochSlots

ftsNoStake
    :: SharedSeed
    -> StakeAndHolder
    -> Bool
ftsNoStake fts (getNoStake -> (txOutAddress, utxo)) =
    let nonEmpty = followTheSatoshi fts utxo
    in not $ elem txOutAddress nonEmpty

-- | This test looks useless, but since transactions with
-- zero coins are not allowed, the Utxo map will never have
-- any addresses with 0 coins to them, meaning a situation
-- where a stakeholder has 100% of stake is one where the
-- map has a single element.
ftsAllStake
    :: SharedSeed
    -> ((TxId, Word32), TxOut)
    -> Bool
ftsAllStake fts (key, t@TxOut{..}) =
    let utxo = M.fromList [(key, t)]
    in all (== txOutAddress) $ followTheSatoshi fts utxo

ftsReasonableStake
    :: Double
    -> Double
    -> FtsStream
    -> StakeAndHolder
    -> Bool
ftsReasonableStake stakeProbability
                   tolerance
		   (getStream -> ftsList)
		   (getNoStake -> (adr, utxo)) =
    let result = go (0,0) ftsList
        errorEstimation = (abs (result - stakeProbability))
    in errorEstimation < tolerance
  where
    totalStake = fromIntegral $ sum $ map (getCoin . txOutValue) $ M.elems utxo
    newStake = round $ (stakeProbability * totalStake) / (1 - stakeProbability)
    key = (unsafeHash ("this is unsafe" :: Text), 0)
    newUtxo = M.insert key (TxOut adr newStake) utxo
    go :: (Int, Int) -> [SharedSeed] -> Double
    go (!p, !t) [] = fromIntegral p / fromIntegral t
    go (!present, !total) (fts : next)
        | total < 100000 =
            if elem adr (followTheSatoshi fts newUtxo)
                then go (1+present, 1+total) next
                else go (present, 1+total) next
        | otherwise = fromIntegral present / fromIntegral total
