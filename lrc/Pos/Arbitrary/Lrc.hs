-- | Arbitrary instances for Lrc types.

module Pos.Arbitrary.Lrc
       ( GenesisMpcThd
       , InvalidRichmenStakes (..)
       , ValidRichmenStakes (..)
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.Reflection (Reifies (..))
import           Test.QuickCheck (Arbitrary (..), Gen, choose, vector)
import           Test.QuickCheck.Arbitrary.Generic (genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Core.Common (Coin, CoinPortion, coinPortionToDouble, mkCoin, unsafeAddCoin,
                                  unsafeGetCoin, unsafeSubCoin)
import           Pos.Core.Configuration (HasGenesisBlockVersionData, genesisBlockVersionData)
import           Pos.Core.Update (BlockVersionData (bvdMpcThd))
import           Pos.Lrc.Types (RichmenStakes)

-- | Wrapper over 'RichmenStakes'. Its 'Arbitrary' instance enforces that the
-- stake distribution inside must be valid with respect to the threshold
-- 'thd', i.e. all of the coins are non-zero and every stakeholder has
-- sufficient coins (above 'coinPortionToDouble thd' %) to participate. Using
-- reflection, the 'thd' phantom type is used to get the threshold desired in
-- each case.
newtype ValidRichmenStakes thd = Valid
    { getValid :: RichmenStakes
    } deriving (Generic, Show, Eq)

instance (Reifies thd CoinPortion) => Arbitrary (ValidRichmenStakes thd) where
    arbitrary = Valid <$> genRichmenStakes (reflect (Proxy @thd))
    shrink = genericShrink

-- | Wrapper over 'RichmenStakes'. Its 'Arbitrary' instance enforces that the
-- stake distribution inside must be invalid, i.e. one of the stakeholders
-- does not have sufficient coins to participate.
newtype InvalidRichmenStakes thd = Invalid
    { getInvalid :: RichmenStakes
    } deriving (Generic, Show, Eq)

instance (Reifies thd CoinPortion) => Arbitrary (InvalidRichmenStakes thd) where
    arbitrary = Invalid <$> do
        validRichmenStakes <- genRichmenStakes (reflect (Proxy @thd))
        poorMan <- arbitrary
        return $ HM.insert poorMan (mkCoin 0) validRichmenStakes
    shrink = genericShrink

genRichmenStakes
    :: CoinPortion -> Gen RichmenStakes
genRichmenStakes thd = do
    let threshold = coinPortionToDouble thd
    let maxN = floor (1 / threshold)
    n <- choose (1, maxN)
    let totalCoins = maxBound @Coin
    let minStake :: Word64
        minStake = ceiling $ threshold * (fromIntegral $ unsafeGetCoin totalCoins)
    let restCoins = unsafeGetCoin $ totalCoins `unsafeSubCoin` (mkCoin $ minStake * fromIntegral n)
    delimiters <- replicateM (n - 1) (choose (0, restCoins))
    let coins = map (unsafeAddCoin $ mkCoin minStake) $ generateCoins restCoins 0 $ sort delimiters
    when (length coins /= n) $
        error "Illegal length of coins"
    stakeholders <- vector n
    pure $ HM.fromList $ zip stakeholders coins
  where
    generateCoins :: Word64 -> Word64 -> [Word64] -> [Coin]
    generateCoins restCoins prev []     = [mkCoin $ restCoins - prev]
    generateCoins restCoins prev (x:xs) = mkCoin (x - prev) : generateCoins restCoins x xs

-- | Utilities moved here to be imported from 'Spec' files needing valid
-- richmen set generation

data GenesisMpcThd

instance HasGenesisBlockVersionData => Reifies GenesisMpcThd CoinPortion where
    reflect _ = bvdMpcThd genesisBlockVersionData
