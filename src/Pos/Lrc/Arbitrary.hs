-- | Arbitrary instances for Lrc types.

module Pos.Lrc.Arbitrary
       ( InvalidRichmenStake (..)
       , ValidRichmenStake (..)
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Pos.Constants       (genesisMpcThd)
import           Pos.Lrc.Types       (RichmenStake)
import           Pos.Types.Address   (StakeholderId)
import           Pos.Types.Core      (Coin, coinPortionDenominator, getCoinPortion,
                                      mkCoin, unsafeGetCoin)
import           Test.QuickCheck     (Arbitrary (..), Gen, choose)

-- | Wrapper over 'RichmenStake'. Its 'Arbitrary' instance enforces that the stake
-- distribution inside must be valid, i.e. all of the coins are non-zero and every
-- stakeholder has sufficient coins to participate.
newtype ValidRichmenStake = Valid
    { getValid :: RichmenStake
    } deriving (Show, Eq)

instance Arbitrary ValidRichmenStake where
    arbitrary = Valid <$> genRichmenStake

-- | Wrapper over 'RichmenStake'. Its 'Arbitrary' instance enforces that the stake
-- distribution inside must be invalid, i.e. one of the stakeholders does not have
-- sufficient coins to participate.
newtype InvalidRichmenStake = Invalid
    { getInvalid :: RichmenStake
    } deriving (Show, Eq)

instance Arbitrary InvalidRichmenStake where
    arbitrary = Invalid <$> do
        validRichmenStake <- genRichmenStake
        poorMan <- arbitrary
        return $ HM.insert poorMan (mkCoin 0) validRichmenStake

genRichmenStake
    :: Gen RichmenStake
genRichmenStake = do
    -- Total number of coins in the 'RichmenStake' hashmap that will be returned. May not
    -- be exactly accurate because of a possible early return in 'fun' (see comment).
    totalCoins <- mkCoin <$> choose (1, unsafeGetCoin maxBound)
    let toD = fromIntegral @Word64 @Double
        -- Minimum percentage of stake required to participate in MPC protocol.
        mpcThreshold = toD (getCoinPortion genesisMpcThd) / (toD coinPortionDenominator)
        -- Since each participant must have 'mpcThreshold' percent of stake to
        -- participate, this number designates the maximum allowed number of stakeholders
        -- with which 'computeSharesDistr' can be successfully called.
        maxRichmen :: Word64
        maxRichmen = floor $ 1 / mpcThreshold
        -- Given the total number of coins in a 'RichmenStake' hashmap, 'maxRichmen'
        -- dictates that there is a minimum amount of stake each holder must have so that
        -- 'computeSharesDistr' can be successful.
        minStake :: Word64
        minStake = ceiling $ mpcThreshold * (fromIntegral $ unsafeGetCoin totalCoins)
        -- Stops either when 'maxRichmen' richmen have been reached, or when presently
        -- available stake is below mpc threshold, in which case it is discarded
        -- and the stakeholder list is returned as is.
        fun :: Word64 -> Word64 -> [(StakeholderId, Coin)] -> Gen RichmenStake
        fun coinAccum richmenNum participants
            | coinAccum < minStake = return $ HM.fromList participants
            | richmenNum == 0 = return $ HM.fromList participants
            | otherwise = do
                richman <- arbitrary
                word <- choose (minStake, coinAccum)
                fun (coinAccum - word)
                    (richmenNum - 1)
                    ((richman, mkCoin word) : participants)
    fun (unsafeGetCoin totalCoins) maxRichmen []
