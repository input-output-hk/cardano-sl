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
import           Pos.Types            (coinPortionToDouble, unsafeIntegerToCoin)
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

genTestnetStakes :: TestStakeOptions -> StakeDistribution
genTestnetStakes TestStakeOptions{..} =
    checkConsistency $ TestnetStakes {..}
  where
    richs = fromIntegral tsoRichmen
    poors = fromIntegral tsoPoors
    testStake = fromIntegral tsoTotalStake

    -- Calculate actual stakes
    desiredRichStake = getShare tsoRichmenShare testStake
    oneRichmanStake = desiredRichStake `div` richs +
        if desiredRichStake `mod` richs > 0 then 1 else 0
    realRichStake = oneRichmanStake * richs
    poorsStake = testStake - realRichStake
    onePoorStake = poorsStake `div` poors
    realPoorStake = onePoorStake * poors

    mpcStake = getShare (coinPortionToDouble Const.genesisMpcThd) testStake

    sdRichmen = fromInteger richs
    sdRichStake = unsafeIntegerToCoin oneRichmanStake
    sdPoor = fromInteger poors
    sdPoorStake = unsafeIntegerToCoin onePoorStake

    -- Consistency checks
    everythingIsConsistent :: [(Bool, Text)]
    everythingIsConsistent =
        [ ( realRichStake + realPoorStake <= testStake
          , "Real rich + poor stake is more than desired."
          )
        , ( oneRichmanStake >= mpcStake
          , "Richman's stake is less than MPC threshold"
          )
        , ( onePoorStake < mpcStake
          , "Poor's stake is more than MPC threshold"
          )
        ]

    checkConsistency :: a -> a
    checkConsistency = case verifyGeneric everythingIsConsistent of
        VerSuccess        -> identity
        VerFailure errors -> error $ formatAllErrors errors

    getShare :: Double -> Integer -> Integer
    getShare sh n = round $ sh * fromInteger n
