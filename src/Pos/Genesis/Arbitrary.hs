
module Pos.Genesis.Arbitrary
       (
       ) where

import           Test.QuickCheck              (Arbitrary (..), choose, oneof)
import           Universum

import qualified Pos.Genesis                  as T
import           Pos.Ssc.GodTossing.Arbitrary ()
import           Pos.Types                    (mkCoin)


instance Arbitrary T.GenesisData where
    arbitrary = T.GenesisData <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary T.StakeDistribution where
    arbitrary = oneof
      [ do stakeholders <- choose (1, 10000)
           coins <- mkCoin <$> choose (stakeholders, 20*1000*1000*1000)
           return (T.FlatStakes (fromIntegral stakeholders) coins)
      , do stakeholders <- choose (1, 10000)
           coins <- mkCoin <$> choose (stakeholders, 20*1000*1000*1000)
           return (T.BitcoinStakes (fromIntegral stakeholders) coins)
      , do sdRichmen <- choose (0, 20)
           sdRichStake <- mkCoin <$> choose (100000, 5000000)
           sdPoor <- choose (0, 20)
           sdPoorStake <- mkCoin <$> choose (1000, 50000)
           return T.RichPoorStakes{..}
      , return T.ExponentialStakes
      , T.ExplicitStakes <$> arbitrary
      ]
