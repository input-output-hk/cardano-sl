-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.Arbitrary
       ( Nonrepeating (..)
       , ArbitraryUnsafe (..)
       , SmallGenerator (..)
       , makeSmall
       , sublistN
       , arbitrarySizedS
       , arbitrarySizedSL
       , runGen
       ) where

import           Universum

import qualified Crypto.Random          as Rand
import           Data.ByteString        (pack)
import qualified Data.ByteString.Lazy   as BL (ByteString, pack)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Formatting             (build, sformat, (%))
import           Pos.Binary.Class       (Bi)
import           Test.QuickCheck        (Arbitrary (..), Gen, listOf, scale, shuffle,
                                         vector)
import           Test.QuickCheck.Gen    (unGen)
import           Test.QuickCheck.Random (mkQCGen)

import           Pos.Crypto.Random      (randomNumberInRange)
import           Pos.Util.Util          ()

makeSmall :: Gen a -> Gen a
makeSmall = scale f
  where
    -- This function is “Golden function of testing”. It is perfect
    -- for making tested values small. There was a profound research
    -- in this area. `f 4` is 3, yes.
    f 0 = 0
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 3
    f n
      | n < 0 = n
      | otherwise =
          (round . (sqrt @Double) . realToFrac . (`div` 3)) n

newtype SmallGenerator a = SmallGenerator
    { getSmallGenerator :: a
    } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (SmallGenerator a) where
    arbitrary = SmallGenerator <$> makeSmall arbitrary
    shrink = fmap SmallGenerator . shrink . getSmallGenerator

deriving instance Bi a => Bi (SmallGenerator a)

-- | Choose a random (shuffled) subset of length n. Throws an error if
-- there's not enough elements.
sublistN :: Rand.MonadRandom m => Int -> [a] -> m [a]
sublistN n xs = do
    let len = length xs
    if len < n then
        error $ sformat ("sublistN: requested "%build%" elements, "%
            "but list only contains "%build) n len
    else do
        seed <- randomNumberInRange 0 (toInteger (maxBound @Int))
        let shuffled = unGen (shuffle xs) (mkQCGen (fromInteger seed)) 30
        pure $ take n shuffled

-- | Type for generating list of unique (nonrepeating) elemets.
class Nonrepeating a where
    nonrepeating :: Int -> Gen [a]

-- | Make arbitrary `ByteString` of given length.
arbitrarySizedS :: Int -> Gen ByteString
arbitrarySizedS n = pack <$> vector n

-- | Make arbitrary `ByteString.Lazy` of given length.
arbitrarySizedSL :: Int -> Gen BL.ByteString
arbitrarySizedSL n = BL.pack <$> vector n

-- | Get something out of a quickcheck 'Gen' without having to do IO
runGen :: Gen a -> a
runGen g = unGen g (mkQCGen 31415926) 30

{-| ArbitraryUnsafe class
    ~~~~~~~~~~~~~~~~~~~~~~~~

    This class is the same as `Arbitrary`, except instances of this class for
    stuff like public/secret keys, VSS shares, commitments etc. are designed
    not to mimic real data as presisely as possible (using OpenSSL random), but
    rather to be simple and efficient.

    This is especially useful for benchmarking.

    Note: we don't need 'Generic' to derive instances of 'ArbitraryUnsafe'.
    We can either use one-line instance declaration, or @-XStandaloneDeriving@
    or @-XDeriveAnyClass@ to write something like @deriving (Arbitrary, ArbitraryUnsafe)@.
-}
class ArbitraryUnsafe a where
    arbitraryUnsafe :: Gen a

    default arbitraryUnsafe :: Arbitrary a => Gen a
    arbitraryUnsafe = arbitrary

instance ArbitraryUnsafe Word16
instance ArbitraryUnsafe Word32
instance ArbitraryUnsafe Word64
instance (Arbitrary ByteString) => ArbitraryUnsafe ByteString

instance ArbitraryUnsafe a => ArbitraryUnsafe [a] where
    arbitraryUnsafe = listOf arbitraryUnsafe

instance ArbitraryUnsafe a => ArbitraryUnsafe (NonEmpty a) where
    arbitraryUnsafe = (:|) <$> arbitraryUnsafe <*> listOf arbitraryUnsafe

instance (ArbitraryUnsafe a, ArbitraryUnsafe b) => ArbitraryUnsafe (a, b) where
    arbitraryUnsafe = (,) <$> arbitraryUnsafe <*> arbitraryUnsafe
