module Testnet
       ( generateKeyfile
       , generateFakeAvvm
       , genTestnetDistribution
       , rearrangeKeyfile
       ) where

import           Universum

import           Control.Lens          ((?~))
import qualified Serokell.Util.Base64  as B64
import           Serokell.Util.Verify  (VerificationRes (..), formatAllErrors,
                                        verifyGeneric)
import           System.Random         (randomRIO)
import           System.Wlog           (WithLogger)

import           Pos.Binary            (asBinary)
import qualified Pos.Constants         as Const
import           Pos.Core              (IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto            (EncryptedSecretKey, PublicKey, RedeemPublicKey,
                                        SecretKey, emptyPassphrase, keyGen, noPassEncrypt,
                                        redeemDeterministicKeyGen, runSecureRandom,
                                        safeKeyGen, secureRandomBS, toPublic,
                                        toVssPublicKey, vssKeyGen)
import           Pos.Genesis           (BalanceDistribution (..), accountGenesisIndex,
                                        wAddressGenesisIndex)
import           Pos.Ssc.GodTossing    (VssCertificate, mkVssCertificate)
import           Pos.Types             (Address, coinPortionToDouble, unsafeIntegerToCoin)
import           Pos.Util.UserSecret   (initializeUserSecret, takeUserSecret, usKeys,
                                        usPrimKey, usVss, usWalletSet,
                                        writeUserSecretRelease)
import           Pos.Wallet.Web.Secret (mkGenesisWalletUserSecret)

import           KeygenOptions         (TestBalanceOptions (..))

rearrangeKeyfile :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrangeKeyfile fp = do
    us <- takeUserSecret fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

generateKeyfile
    :: (MonadIO m, MonadFail m, WithLogger m)
    => Bool
    -> Maybe (SecretKey, EncryptedSecretKey)  -- ^ plain key & hd wallet root key
    -> FilePath
    -> m (PublicKey, VssCertificate, Address)  -- ^ plain key, certificate & hd wallet
                                               -- account address with bootstrap era distribution
generateKeyfile isPrim mbSk fp = do
    initializeUserSecret fp
    (sk, hdwSk) <- case mbSk of
        Just x  -> return x
        Nothing -> liftIO $ runSecureRandom $
            (,) <$> (snd <$> keyGen)
                <*> (snd <$> safeKeyGen emptyPassphrase)
    vss <- liftIO $ runSecureRandom vssKeyGen
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
        -- This address is used only to create genesis data. We don't
        -- put it into a keyfile.
        hdwAccountPk =
            fst $ fromMaybe (error "generateKeyfile: pass mismatch") $
            deriveLvl2KeyPair (IsBootstrapEraAddr True) emptyPassphrase hdwSk
                accountGenesisIndex wAddressGenesisIndex
    return (toPublic sk, vssCert, hdwAccountPk)

generateFakeAvvm :: MonadIO m => FilePath -> m RedeemPublicKey
generateFakeAvvm fp = do
    seed <- secureRandomBS 32
    let (pk, _) = fromMaybe
            (error "cardano-keygen: impossible - seed is not 32 bytes long") $
            redeemDeterministicKeyGen seed
    writeFile fp $ B64.encode seed
    return pk

-- | Generates balance distribution for testnet.
genTestnetDistribution :: TestBalanceOptions -> BalanceDistribution
genTestnetDistribution TestBalanceOptions{..} =
    checkConsistency $ RichPoorBalances {..}
  where
    richs = fromIntegral tsoRichmen
    poors = fromIntegral tsoPoors * 2  -- for plain and hd wallet keys
    testBalance = fromIntegral tsoTotalBalance

    -- Calculate actual balances
    desiredRichBalance = getShare tsoRichmenShare testBalance
    oneRichmanBalance = desiredRichBalance `div` richs +
        if desiredRichBalance `mod` richs > 0 then 1 else 0
    realRichBalance = oneRichmanBalance * richs
    poorsBalance = testBalance - realRichBalance
    onePoorBalance = poorsBalance `div` poors
    realPoorBalance = onePoorBalance * poors

    mpcBalance = getShare (coinPortionToDouble Const.genesisMpcThd) testBalance

    sdRichmen = fromInteger richs
    sdRichBalance = unsafeIntegerToCoin oneRichmanBalance
    sdPoor = fromInteger poors
    sdPoorBalance = unsafeIntegerToCoin onePoorBalance

    -- Consistency checks
    everythingIsConsistent :: [(Bool, Text)]
    everythingIsConsistent =
        [ ( realRichBalance + realPoorBalance <= testBalance
          , "Real rich + poor balance is more than desired."
          )
        , ( oneRichmanBalance >= mpcBalance
          , "Richman's balance is less than MPC threshold"
          )
        , ( onePoorBalance < mpcBalance
          , "Poor's balance is more than MPC threshold"
          )
        ]

    checkConsistency :: a -> a
    checkConsistency = case verifyGeneric everythingIsConsistent of
        VerSuccess        -> identity
        VerFailure errors -> error $ formatAllErrors errors

    getShare :: Double -> Integer -> Integer
    getShare sh n = round $ sh * fromInteger n
