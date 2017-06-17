-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import           Data.Default          (def)
import           Data.List             (scanl1)
import qualified Data.Map              as M (fromList, insert, singleton)
import qualified Data.Set              as S (deleteFindMin, fromList, size)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), choose, infiniteListOf, suchThat)
import           Universum

import           Pos.Constants         (epochSlots)
import           Pos.Core              (Address (..), Coin, SharedSeed, StakeholderId,
                                        mkCoin, sumCoins, unsafeAddCoin,
                                        unsafeIntegerToCoin)
import           Pos.Crypto            (unsafeHash)
import           Pos.Lrc               (followTheSatoshiUtxo)
import           Pos.Txp               (TxIn (..), TxOut (..), TxOutAux (..), Utxo,
                                        txOutStake)

spec :: Spec
spec = do
    let smaller = modifyMaxSuccess (const 1)
    describe "FollowTheSatoshi" $ do
        describe "followTheSatoshiUtxo" $ do
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
    acceptable x y = and [x >= y * 0.85, x <= y * 1.15]
    lowStakeTolerance (pLen, present, chosen) =
        acceptable present (1 - (1 - lowStake) ^ pLen) &&
        acceptable chosen lowStake
    highStakeTolerance (pLen, present, chosen) =
        acceptable present (1 - (1 - highStake) ^ pLen) &&
        acceptable chosen highStake

-- | Type used to generate random Utxo and a pubkey hash (addrhash) that is
-- not in this map, meaning it does not hold any stake in the system's
-- current state.
--
-- Two necessarily different addrhashes are generated, as well as a list of
-- addrhashes who will be our other stakeholders. To guarantee a non-empty
-- utxo map, one of these addrhashes is inserted in the list, which is
-- converted to a set and then to a map, where each addrhash is given as key
-- a random pair (TxId, Coin).
newtype StakeAndHolder = StakeAndHolder
    { getNoStake :: (StakeholderId, Utxo)
    } deriving Show

instance Arbitrary StakeAndHolder where
    arbitrary = StakeAndHolder <$> do
        addrHash1 <- arbitrary
        addrHash2 <- arbitrary `suchThat` ((/=) addrHash1)
        listAdr <- do
            n <- choose (2, 10)
            replicateM n arbitrary
        txId <- arbitrary
        coins <- mkCoin <$> choose (1, 1000)
        let setAdr = S.fromList $ addrHash1 : addrHash2 : listAdr
            (myAddrHash, setUtxo) = S.deleteFindMin setAdr
            nAdr = S.size setUtxo
            values = scanl1 unsafeAddCoin $ replicate nAdr coins
            toTxOutAux ah v = TxOutAux (TxOut (PubKeyAddress ah def) v) []
            utxoList =
                (zipWith TxIn (replicate nAdr txId) [0 .. fromIntegral nAdr]) `zip`
                (zipWith toTxOutAux (toList setUtxo) values)
        return (myAddrHash, M.fromList utxoList)

ftsListLength :: SharedSeed -> StakeAndHolder -> Bool
ftsListLength fts (getNoStake -> (_, utxo)) =
    length (followTheSatoshiUtxo fts utxo) == epochSlots

ftsNoStake
    :: SharedSeed
    -> StakeAndHolder
    -> Bool
ftsNoStake fts (getNoStake -> (addrHash, utxo)) =
    not (addrHash `elem` followTheSatoshiUtxo fts utxo)

-- | This test looks useless, but since transactions with zero coins are not
-- allowed, the Utxo map will never have any addresses with 0 coins to them,
-- meaning a situation where a stakeholder has 100% of stake is one where the
-- map has a single element.
ftsAllStake
    :: SharedSeed
    -> TxIn
    -> StakeholderId
    -> Coin
    -> Bool
ftsAllStake fts key ah v =
    let utxo = M.singleton key (TxOutAux (TxOut (PubKeyAddress ah def) v) [])
    in all (== ah) $ followTheSatoshiUtxo fts utxo

-- | Constant specifying the number of times 'ftsReasonableStake' will be
-- run.
numberOfRuns :: Num a => a
numberOfRuns = 3000

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

-- | This test is a sanity check to verify that 'followTheSatoshiUtxo' does not
-- behave too extremely, i.e. someone with 2% of stake won't be chosen a
-- disproportionate number of times, and someone with 98% of it will be
-- chosen almost every time.
--
-- For an infinite list of Utxo maps and an infinite list of 'SharedSeed's, the
-- 'followTheSatoshiUtxo' function will be ran many times with a different seed and
-- map each time and the absolute frequency of the choice of a given address
-- as stakeholder will be compared to a low/high threshold, depending on whether
-- the address has a low/high stake, respectively.
-- For a low/high stake, the test succeeds if this comparison is below/above the
-- threshold, respectively.
ftsReasonableStake
    :: Double
    -> ((Int, Double, Double) -> Bool)
    -> FtsStream
    -> UtxoStream
    -> Bool
ftsReasonableStake
    stakeProbability
    threshold
    (getStream     -> ftsList)
    (getUtxoStream -> utxoList)
  =
    let result = go numberOfRuns (0, 0, 0) ftsList utxoList
    in threshold result
  where
    key = TxIn (unsafeHash ("this is unsafe" :: Text)) 0

    -- We count how many times someone was present in selection and how many
    -- times someone was chosen overall.
    go :: Int
       -> (Int, Double, Double)
       -> [SharedSeed]
       -> [StakeAndHolder]
       -> (Int, Double, Double)
    go 0 (pl, p, c)  _  _ = (pl, p, c)
    go _ (pl, p, c) []  _ = (pl, p, c)
    go _ (pl, p, c)  _ [] = (pl, p, c)
    go total (_, !present, !chosen) (fts : nextSeed) (u : nextUtxo) =
        go (total - 1) (pLen, newPresent, newChosen) nextSeed nextUtxo
      where
        (adrH, utxo) = getNoStake u
        totalStake   = fromIntegral . sumCoins . map snd $
                       concatMap txOutStake (toList utxo)
        newStake     = unsafeIntegerToCoin . round $
                           (stakeProbability * totalStake) /
                           (1 - stakeProbability)
        txOut        = TxOut (PubKeyAddress adrH def) newStake
        newUtxo      = M.insert key (TxOutAux txOut []) utxo
        picks        = followTheSatoshiUtxo fts newUtxo
        pLen         = length picks
        newPresent   = present +
            if adrH `elem` picks then 1 / numberOfRuns else 0
        newChosen    = chosen +
            fromIntegral (length (filter (== adrH) (toList picks))) /
            (numberOfRuns * fromIntegral pLen)
