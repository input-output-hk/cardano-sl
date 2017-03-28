module Testnet
       ( generateKeyfile
       , genTestnetStakes
       ) where

import           Serokell.Util.Verify (VerificationRes (..), formatAllErrors,
                                       verifyGeneric)
import           System.Random        (randomRIO)
import           Universum

import           Pos.Binary           (asBinary)
import qualified Pos.Constants        as Const
import           Pos.Crypto           (PublicKey, keyGen, toPublic, toVssPublicKey,
                                       vssKeyGen)
import           Pos.Genesis          (StakeDistribution (..))
import           Pos.Ssc.GodTossing   (VssCertificate, mkVssCertificate)
import           Pos.Types            (Coin, coinPortionToDouble, coinToInteger,
                                       unsafeIntegerToCoin)
import           Pos.Util.UserSecret  (initializeUserSecret, takeUserSecret, usPrimKey,
                                       usVss, writeUserSecretRelease)

import           KeygenOptions        (TestStakeOptions (..))

generateKeyfile :: FilePath -> IO (PublicKey, VssCertificate)
generateKeyfile fp = do
    initializeUserSecret fp
    sk <- snd <$> keyGen
    vss <- vssKeyGen
    us <- takeUserSecret fp
    writeUserSecretRelease $
        us & usPrimKey .~ Just sk
           & usVss .~ Just vss
    expiry <- fromIntegral <$> randomRIO (Const.vssMinTTL :: Int, Const.vssMaxTTL)
    let vssPk = asBinary $ toVssPublicKey vss
        vssCert = mkVssCertificate sk vssPk expiry
    return (toPublic sk, vssCert)

genTestnetStakes :: Coin -> TestStakeOptions -> StakeDistribution
genTestnetStakes avvmStake TestStakeOptions{..} = checkConsistency $ TestnetStakes {..}
  where
    richs = fromIntegral tsoRichmen
    poors = fromIntegral tsoPoors
    testStake = fromIntegral tsoTotalStake
    totalStake = coinToInteger avvmStake + testStake

    -- Calculate actual stakes
    maxRichStake = testStake - poors
    desiredRichStake = getShare tsoRichmenShare totalStake
    oneRichmanStake = desiredRichStake `div` richs +
        if desiredRichStake `mod` richs > 0 then 1 else 0
    realRichStake = oneRichmanStake * richs
    poorsStake = testStake - realRichStake
    onePoorStake = poorsStake `div` poors
    mpcStake = getShare (coinPortionToDouble Const.genesisMpcThd) totalStake

    sdRichmen = fromInteger richs
    sdRichStake = unsafeIntegerToCoin oneRichmanStake
    sdPoor = fromInteger poors
    sdPoorStake = unsafeIntegerToCoin onePoorStake

    -- Consistency checks
    everythingIsConsistent :: [(Bool, Text)]
    everythingIsConsistent =
        [ ( maxRichStake <= realRichStake
          , "Desired richmen's stake is more than allowed by \
            \constrains on total stake and given AVVM stake."
          )
        , ( oneRichmanStake >= mpcStake
          , "Richman's stake is less than MPC threshold"
          )
        , ( poorsStake < mpcStake
          , "Poor's stake is more than MPC threshold"
          )
        ]

    checkConsistency :: a -> a
    checkConsistency = case verifyGeneric everythingIsConsistent of
        VerSuccess        -> identity
        VerFailure errors -> error $ formatAllErrors errors

    getShare :: Double -> Integer -> Integer
    getShare sh n = round $ sh * fromInteger n

