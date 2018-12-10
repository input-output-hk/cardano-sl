module Test.Pos.Chain.Lrc.StakeAndHolder
       ( StakeAndHolder (..)
       ) where

import           Universum

import           Data.List (scanl1)
import qualified Data.Set as S (deleteFindMin, fromList)
import           Test.QuickCheck (Arbitrary (..), choose, suchThat)

import           Pos.Core (StakesList, addressHash, mkCoin, unsafeAddCoin)
import           Pos.Crypto (PublicKey)

import           Test.Pos.Core.Arbitrary ()

-- | Type used to generate random stakes and a 'PublicKey' that
-- doesn't have any stake.
--
-- Two necessarily different public keys are generated, as well as a list of
-- public keys who will be our other stakeholders. To guarantee a non-empty
-- stakes map, one of these public keys is inserted in the list, which is
-- converted to a set and then to a map.
newtype StakeAndHolder = StakeAndHolder
    { getNoStake :: (PublicKey, StakesList)
    } deriving Show

instance Arbitrary StakeAndHolder where
    arbitrary = StakeAndHolder <$> do
        pk1 <- arbitrary
        pk2 <- arbitrary `suchThat` ((/=) pk1)
        listPks <- do
            n <- choose (2, 10)
            replicateM n arbitrary
        coins <- mkCoin <$> choose (1, 1000)
        let setPks :: Set PublicKey
            setPks = S.fromList $ pk1 : pk2 : listPks
            (myPk, restPks) = S.deleteFindMin setPks
            nRest = length restPks
            values = scanl1 unsafeAddCoin $ replicate nRest coins
            stakesList = map addressHash (toList restPks) `zip` values
        return (myPk, stakesList)
