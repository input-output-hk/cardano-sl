{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import           Data.List             (scanl1)
import qualified Data.Map              as M (elems, fromList, insert)
import qualified Data.Set              as S (deleteFindMin, fromList, size, toList)
import           Pos.Constants         (epochSlots)
import           Pos.Crypto            (unsafeHash)
import           Pos.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Types             (Address, Coin (..), SharedSeed, TxId, TxOut (..),
                                        Utxo)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, infiniteListOf, suchThat)
import           Universum

spec :: Spec
spec = do
    let smaller = modifyMaxSuccess (const 1)
    describe "FollowTheSatoshi" $ do
        describe "followTheSatoshi" $ do
            describe "deterministic" $ do
                prop description_ftsListLength ftsListLength
                prop description_ftsNoStake ftsNoStake
                prop description_ftsAllStake ftsAllStake
            describe "probabilistic" $ smaller $ do
                prop description_ftsLowStake
                    (ftsReasonableStake lowStake lowStakeTolerance)
                prop description_ftsHighStake
                    (ftsReasonableStake highStake highStakeTolerance)
  where
    description_ftsListLength =
        "the amount of stakeholders is the same as the number of slots in an epoch"
    description_ftsNoStake =
        "a stakeholder with 0% stake won't ever be selected as slot leader"
    description_ftsAllStake =
        "a stakeholder with 100% stake will always be selected as slot leader, and he \
        \ will be the only stakeholder"
    description_ftsLowStake =
        "a stakeholder with low stake will be chosen seldom"
    description_ftsHighStake =
        "a stakeholder with high stake will be chosen often"
    lowStake  = 0.02
    highStake = 0.98
    lowStakeTolerance  = (< round (fromIntegral numberOfRuns * lowStake)  + 50)  -- < ~250
    highStakeTolerance = (> round (fromIntegral numberOfRuns * highStake) - 50)  -- > ~9750

-- | Type used to generate random Utxo and an address that is not in this map,
-- meaning it does not hold any stake in the system's current state.
--
-- Two necessarily different addresses are generated, as well as a list of
-- addresses who will be our other stakeholders. To guarantee a non-empty utxo
-- map, one these addresses is inserted in the list, which is converted to a
-- set and then to a map, where each address is given as key a random pair
-- (TxId, Coin).
newtype StakeAndHolder = StakeAndHolder
    { getNoStake :: (Address, Utxo)
    } deriving Show

instance Arbitrary StakeAndHolder where
    arbitrary = StakeAndHolder <$> do
        adr1 <- arbitrary
        adr2 <- arbitrary `suchThat` ((/=) adr1)
        listAdr <- arbitrary :: Gen [Address]
        txId <- arbitrary
        coins <- arbitrary :: Gen Coin
        let setAdr = S.fromList $ adr1 : adr2 : listAdr
            (myAdr, setUtxo) = S.deleteFindMin setAdr
            nAdr = S.size setUtxo
            values = scanl1 (+) $ replicate nAdr coins
            utxoList =
                (replicate nAdr txId `zip` [0 .. fromIntegral nAdr]) `zip`
                (zipWith (flip TxOut) values $ S.toList setUtxo)
        return (myAdr, M.fromList utxoList)

ftsListLength :: SharedSeed -> StakeAndHolder -> Bool
ftsListLength fts (getNoStake -> (_, utxo)) =
    (length $ followTheSatoshi fts utxo) == epochSlots

ftsNoStake
    :: SharedSeed
    -> StakeAndHolder
    -> Bool
ftsNoStake fts (getNoStake -> (txOutAddress, utxo)) =
    let nonEmpty = followTheSatoshi fts utxo
    in notElem txOutAddress nonEmpty

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

-- | Constant specifying the number of times 'ftsReasonableStake' will be run.
numberOfRuns :: Int
numberOfRuns = 10000

newtype FtsStream = Stream
    { getStream :: [SharedSeed]
    } deriving Show

instance Arbitrary FtsStream where
    arbitrary = Stream . take numberOfRuns <$> infiniteListOf arbitrary

newtype UtxoStream = UtxoStream
    { getUtxoStream :: [StakeAndHolder]
    } deriving Show

instance Arbitrary UtxoStream where
    arbitrary = UtxoStream . take numberOfRuns <$> infiniteListOf arbitrary

-- | This test is a sanity check to verify that 'followTheSatoshi' does not
-- behave too extremely, i.e. someone with 2% of stake won't be chosen a
-- disproportionate number of times, and someone with 98% of it will be
-- chosen almost every time.
--
-- For an infinite list of Utxo maps and an infinite list of 'SharedSeed's, the
-- 'followTheSatoshi' function will be ran many times with a different seed and
-- map each time and the absolute frequency of the choice of a given address
-- as stakeholder will be compared to a low/high threshold, depending on whether
-- the address has a low/high stake, respectively.
-- For a low/high stake, the test succeeds if this comparison is below/above the
-- threshold, respectively.
ftsReasonableStake
    :: Double
    -> (Word -> Bool)
    -> FtsStream
    -> UtxoStream
    -> Bool
ftsReasonableStake
    stakeProbability
    threshold
    (getStream     -> ftsList)
    (getUtxoStream -> utxoList)
  =
    let result = go numberOfRuns 0 ftsList utxoList
    in threshold result
  where
    key = (unsafeHash ("this is unsafe" :: Text), 0)

    go :: Int -> Word -> [SharedSeed] -> [StakeAndHolder] -> Word
    go 0 p  _  _ = p
    go _ p []  _ = p
    go _ p  _ [] = p
    go total !present (fts : nextSeed) ((getNoStake -> (adr, utxo)) : nextUtxo) =
        let totalStake = fromIntegral $ sum $ map (getCoin . txOutValue) $ M.elems utxo
            newStake   = round $ (stakeProbability * totalStake) / (1 - stakeProbability)
            newUtxo    = M.insert key (TxOut adr newStake) utxo
            newPresent = if elem adr (followTheSatoshi fts newUtxo)
                         then present + 1
                         else present
        in go (total - 1) newPresent nextSeed nextUtxo
