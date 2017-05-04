module Testnet
       ( generateKeyfile
       , generateHdwKeyfile
       , generateFakeAvvm
       , genTestnetStakes
       , rearrangeKeyfile
       ) where

import qualified Serokell.Util.Base64 as B64
import           Serokell.Util.Verify (VerificationRes (..), formatAllErrors,
                                       verifyGeneric)
import           System.Random        (randomRIO)
import           System.Wlog          (WithLogger)
import           Universum

import           Pos.Binary           (asBinary)
import qualified Pos.Constants        as Const
import           Pos.Crypto           (EncryptedSecretKey, PublicKey, RedeemPublicKey,
                                       SecretKey, keyGen, noPassEncrypt,
                                       redeemDeterministicKeyGen, secureRandomBS,
                                       toPublic, toVssPublicKey, vssKeyGen)
import           Pos.Genesis          (StakeDistribution (..), accountGenesisIndex,
                                       walletGenesisIndex)
import           Pos.Ssc.GodTossing   (VssCertificate, mkVssCertificate)
import           Pos.Types            (coinPortionToDouble, unsafeIntegerToCoin)
import           Pos.Util.UserSecret  (initializeUserSecret, takeUserSecret, usKeys,
                                       usPrimKey, usVss, writeUserSecretRelease)
import           Pos.Wallet.Web       (WalletUserSecret (..), writeWalletUserSecret)

import           KeygenOptions        (TestStakeOptions (..))

rearrangeKeyfile :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrangeKeyfile fp = do
    us <- takeUserSecret fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

generateKeyfile
    :: (MonadIO m, MonadFail m, WithLogger m)
    => Bool -> Maybe SecretKey -> FilePath -> m (PublicKey, VssCertificate)
generateKeyfile isPrim mbSk fp = do
    initializeUserSecret fp
    sk <- case mbSk of
        Just x  -> return x
        Nothing -> snd <$> keyGen
    vss <- vssKeyGen
    us <- takeUserSecret fp
    writeUserSecretRelease $
        us & (if isPrim
              then usPrimKey .~ Just sk
              else usKeys %~ (noPassEncrypt sk :))
           & usVss .~ Just vss
    expiry <- liftIO $
        fromIntegral <$>
        randomRIO @Int (Const.vssMinTTL - 1, Const.vssMaxTTL - 1)
    let vssPk = asBinary $ toVssPublicKey vss
        vssCert = mkVssCertificate sk vssPk expiry
    return (toPublic sk, vssCert)

generateHdwKeyfile
    :: (MonadIO m, MonadFail m, WithLogger m)
    => EncryptedSecretKey -> FilePath -> m ()
generateHdwKeyfile wusRootKey fp = do
    let wusWSetName = "Genesis wallet set"
        wusWallets  = [(walletGenesisIndex, "Genesis wallet")]
        wusAccounts = [(walletGenesisIndex, accountGenesisIndex)]
    writeWalletUserSecret fp WalletUserSecret{..}

generateFakeAvvm :: MonadIO m => FilePath -> m RedeemPublicKey
generateFakeAvvm fp = do
    seed <- secureRandomBS 32
    let (pk, _) = fromMaybe
            (error "cardano-keygen: impossible - seed is not 32 bytes long") $
            redeemDeterministicKeyGen seed
    writeFile fp $ B64.encode seed
    return pk

genTestnetStakes :: TestStakeOptions -> StakeDistribution
genTestnetStakes TestStakeOptions{..} =
    checkConsistency $ RichPoorStakes {..}
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
