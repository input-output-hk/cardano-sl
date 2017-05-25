module Testnet
       ( generateKeyfile
       , generateFakeAvvm
       , genTestnetStakes
       , rearrangeKeyfile
       ) where

import           Control.Lens         ((?~))
import qualified Serokell.Util.Base64 as B64
import           Serokell.Util.Verify (VerificationRes (..), formatAllErrors,
                                       verifyGeneric)
import           System.Random        (randomRIO)
import           System.Wlog          (WithLogger)
import           Universum

import           Pos.Binary           (asBinary)
import qualified Pos.Constants        as Const
import           Pos.Crypto           (EncryptedSecretKey, PublicKey, RedeemPublicKey,
                                       SecretKey, emptyPassphrase, keyGen, noPassEncrypt,
                                       redeemDeterministicKeyGen, safeKeyGen,
                                       secureRandomBS, toPublic, toVssPublicKey,
                                       vssKeyGen)
import           Pos.Genesis          (StakeDistribution (..), accountGenesisIndex,
                                       walletGenesisIndex)
import           Pos.Ssc.GodTossing   (VssCertificate, mkVssCertificate)
import           Pos.Types            (Address, coinPortionToDouble, unsafeIntegerToCoin)
import           Pos.Util.UserSecret  (initializeUserSecret, takeUserSecret, usKeys,
                                       usPrimKey, usVss, usWalletSet,
                                       writeUserSecretRelease)
import           Pos.Wallet           (WalletUserSecret (..), deriveLvl2KeyPair)

import           KeygenOptions        (TestStakeOptions (..))

rearrangeKeyfile :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrangeKeyfile fp = do
    us <- takeUserSecret fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

generateKeyfile
    :: (MonadIO m, MonadFail m, WithLogger m)
    => Bool
    -> Maybe (SecretKey, EncryptedSecretKey)  -- plain key & hd wallet root key
    -> FilePath
    -> m (PublicKey, VssCertificate, Address)  -- ^ plain key, certificate & hd wallet account address
generateKeyfile isPrim mbSk fp = do
    initializeUserSecret fp
    (sk, hdwSk) <- case mbSk of
        Just x  -> return x
        Nothing -> (,) <$> (snd <$> keyGen) <*> (snd <$> safeKeyGen emptyPassphrase)
    vss <- vssKeyGen
    us <- takeUserSecret fp

    writeUserSecretRelease $
        us & (if isPrim
              then usPrimKey .~ Just sk
              else (usKeys %~ (noPassEncrypt sk :))
                 . (usWalletSet ?~ mkGenesisWalletUserSecret hdwSk))
           & usVss .~ Just vss

    expiry <- liftIO $
        fromIntegral <$>
        randomRIO @Int (Const.vssMinTTL - 1, Const.vssMaxTTL - 1)
    let vssPk = asBinary $ toVssPublicKey vss
        vssCert = mkVssCertificate sk vssPk expiry
        hdwAccountPk =
            fst $ fromMaybe (error "generateKeyfile: pass mismatch") $
            deriveLvl2KeyPair emptyPassphrase hdwSk
                walletGenesisIndex accountGenesisIndex
    return (toPublic sk, vssCert, hdwAccountPk)

mkGenesisWalletUserSecret
    :: EncryptedSecretKey -> WalletUserSecret
mkGenesisWalletUserSecret wusRootKey = do
    let wusWSetName = "Genesis wallet set"
        wusWallets  = [(walletGenesisIndex, "Genesis wallet")]
        wusAccounts = [(walletGenesisIndex, accountGenesisIndex)]
    WalletUserSecret{..}

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
    poors = fromIntegral tsoPoors * 2  -- for plain and hd wallet keys
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
