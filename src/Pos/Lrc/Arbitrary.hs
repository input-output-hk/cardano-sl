{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Arbitrary instances for Lrc types.

module Pos.Lrc.Arbitrary
       ( InvalidRichmenStake (..)
       , ValidRichmenStake (..)
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.Reflection     (Reifies (..))
import           Pos.Lrc.Types       (RichmenStake)
import           Pos.Types.Address   (StakeholderId)
import           Pos.Types.Coin      (coinPortionToDouble)
import           Pos.Types.Core      (Coin, CoinPortion, mkCoin, unsafeGetCoin)
import           Test.QuickCheck     (Arbitrary (..), Gen, choose)

-- | Wrapper over 'RichmenStake'. Its 'Arbitrary' instance enforces that the stake
-- distribution inside must be valid with respect to the threshold 'thd', i.e. all of the
-- coins are non-zero and every stakeholder has sufficient coins
-- (above 'coinPortionToDouble thd' %) to participate.
-- Using reflection, the 'thd' phantom type is used to get the threshold desired in each
-- case.
newtype ValidRichmenStake thd = Valid
    { getValid :: RichmenStake
    } deriving (Show, Eq)

instance (Reifies thd CoinPortion) => Arbitrary (ValidRichmenStake thd) where
    arbitrary = Valid <$> genRichmenStake (reflect (Proxy @thd))

-- | Wrapper over 'RichmenStake'. Its 'Arbitrary' instance enforces that the stake
-- distribution inside must be invalid, i.e. one of the stakeholders does not have
-- sufficient coins to participate.
newtype InvalidRichmenStake thd = Invalid
    { getInvalid :: RichmenStake
    } deriving (Show, Eq)

instance (Reifies thd CoinPortion) => Arbitrary (InvalidRichmenStake thd) where
    arbitrary = Invalid <$> do
        validRichmenStake <- genRichmenStake (reflect (Proxy @thd))
        poorMan <- arbitrary
        return $ HM.insert poorMan (mkCoin 0) validRichmenStake

genRichmenStake
    :: CoinPortion -> Gen RichmenStake
genRichmenStake thd = do
    -- Total number of coins in the 'RichmenStake' hashmap that will be returned. May not
    -- be exactly accurate because of a possible early return in 'fun' (see comment).
    totalCoins <- mkCoin <$> choose (1, unsafeGetCoin maxBound)
    let -- Minimum percentage of stake required to be a richmen.
        threshold = coinPortionToDouble thd
        -- Since each participant must have 'mpcThreshold' percent of stake to
        -- participate, this number designates the maximum allowed number of stakeholders
        -- with which 'computeSharesDistr' can be successfully called.
        maxRichmen :: Word64
        maxRichmen = floor $ 1 / threshold
        -- Given the total number of coins in a 'RichmenStake' hashmap, 'threshold'
        -- dictates that there is a minimum percentage of stake each holder must have so
        -- that 'computeSharesDistr' can be successful. 'minStake' is that minimum value,
        -- but expressed as a 'Word64'.
        minStake :: Word64
        minStake = ceiling $ threshold * (fromIntegral $ unsafeGetCoin totalCoins)
        -- Stops either when 'maxRichmen' richmen have been reached, or when presently
        -- available stake is below mpc threshold, in which case it is discarded
        -- and the stakeholder list is returned as is.
        fun :: Word64 -> Word64 -> [(StakeholderId, Coin)] -> Gen RichmenStake
        fun coinAccum richmenNum participants
            | coinAccum < minStake = return $ HM.fromList participants
            | otherwise = do
                richman <- arbitrary
                word <- choose (minStake, coinAccum)
                fun (coinAccum - word)
                    (richmenNum - 1)
                    ((richman, mkCoin word) : participants)
    fun (unsafeGetCoin totalCoins) maxRichmen []
