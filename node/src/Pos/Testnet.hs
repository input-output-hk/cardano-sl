-- | Generation of genesis data for testnet.

module Pos.Testnet
       ( genTestnetData
       , genFakeAvvmGenesis

       , generateKeyfile
       , generateFakeAvvm
       , genTestnetOrMainnetData
       ) where

import           Universum

import           Control.Lens               ((?~))
import           Crypto.Random              (getRandomBytes)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map.Strict            as Map
import           Formatting                 (sformat, shown, (%))
import qualified Serokell.Util.Base64       as B64
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (VerificationRes (..), formatAllErrors,
                                             verifyGeneric)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))
import           System.Wlog                (WithLogger, logInfo)

import           Pos.Binary                 (asBinary)
import qualified Pos.Constants              as Const
import           Pos.Core                   (IsBootstrapEraAddr (..), VssCertificate,
                                             addressDetailedF, addressHash,
                                             deriveLvl2KeyPair, makePubKeyAddressBoot,
                                             makeRedeemAddress, mkCoin, mkVssCertificate)
import           Pos.Core.Genesis           (AddrDistribution, BalanceDistribution (..),
                                             FakeAvvmOptions (..),
                                             GenesisInitializer (..),
                                             GenesisWStakeholders (..),
                                             TestnetBalanceOptions (..),
                                             TestnetDistribution (..))
import           Pos.Crypto                 (EncryptedSecretKey, PublicKey,
                                             RedeemPublicKey, SecretKey, emptyPassphrase,
                                             keyGen, noPassEncrypt, randomNumberInRange,
                                             redeemDeterministicKeyGen, runGlobalRandom,
                                             safeKeyGen, setGlobalRandomSeed, toPublic,
                                             toVssPublicKey, vssKeyGen)
import           Pos.Ssc.GodTossing.Genesis (GenesisGtData (..))
import           Pos.Types                  (Address, coinPortionToDouble,
                                             unsafeIntegerToCoin)
import           Pos.Util.UserSecret        (initializeUserSecret, takeUserSecret, usKeys,
                                             usPrimKey, usVss, usWalletSet,
                                             writeUserSecretRelease)
import           Pos.Util.Util              (applyPattern)
import           Pos.Wallet.Web.Secret      (accountGenesisIndex,
                                             mkGenesisWalletUserSecret,
                                             wAddressGenesisIndex)


genTestnetOrMainnetData
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => GenesisInitializer
    -> m ([AddrDistribution], GenesisWStakeholders, GenesisGtData)
genTestnetOrMainnetData TestnetInitializer{..} = do
    liftIO (setGlobalRandomSeed tiSeed)
    fakeAvvmDistr <- genFakeAvvmGenesis Nothing tiFakeAvvmBalance
    (_1 %~ (++fakeAvvmDistr)) <$> genTestnetData Nothing tiTestBalance tiDistribution
genTestnetOrMainnetData MainnetInitializer{..} =
    pure ([], miBootStakeholders, GenesisGtData miVssCerts)

-- Generates keys and vss certs for testnet data. Returns:
-- 1. Address distribution
-- 2. Set of boot stakeholders (richmen addresses)
-- 3. Genesis vss data (vss certs of richmen)
genTestnetData
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => Maybe (FilePath, FilePath) -- directory and key-file pattern
    -> TestnetBalanceOptions
    -> TestnetDistribution
    -> m ([AddrDistribution], GenesisWStakeholders, GenesisGtData)
genTestnetData dirPatMB tso@TestnetBalanceOptions{..} distrSpec = do
    -- We always want to have bootstrap era distribution in genesis addresses.
    let isBoot = IsBootstrapEraAddr True

    (richmenList, poorsList) <-
        case dirPatMB of
            Just (dir, pat) -> do
                let keysDir = dir </> "keys-testnet"
                let richDir = keysDir </> "rich"
                let poorDir = keysDir </> "poor"
                logInfo $ "Generating testnet data into " <> fromString keysDir
                liftIO $ createDirectoryIfMissing True richDir
                liftIO $ createDirectoryIfMissing True poorDir

                let totalStakeholders = tboRichmen + tboPoors

                richmenList <- forM [1 .. tboRichmen] $ \i ->
                    generateKeyfile True Nothing (Just (richDir </> applyPattern pat i))
                poorsList <- forM [1 .. tboPoors] $ \i ->
                    generateKeyfile False Nothing (Just (poorDir </> applyPattern pat i))

                logInfo $ show totalStakeholders <> " keyfiles are generated"
                pure (richmenList, poorsList)
            Nothing -> (,) <$> replicateM (fromIntegral tboRichmen) (generateKeyfile' isBoot True Nothing Nothing)
                           <*> replicateM (fromIntegral tboPoors)   (generateKeyfile' isBoot False Nothing Nothing)

    let genesisList = map (\(k, vc, _) -> (k, vc)) $ richmenList ++ poorsList
    let genesisListRich = take (fromIntegral tboRichmen) genesisList

    let distr = genTestnetDistribution tso
        genesisAddrs = map (makePubKeyAddressBoot . fst) genesisList
                    <> map (view _3) poorsList
        genesisAddrDistr = [(genesisAddrs, distr)]

    case distr of
        RichPoorBalances {} -> pass
        _                   -> error "Impossible type of generated testnet balance"
    let toStakeholders = Map.fromList . map ((,1) . addressHash . fst)
    let toVss = HM.fromList . map (_1 %~ addressHash)

    let (bootStakeholders, gtData) =
            case distrSpec of
                TestnetRichmenStakeDistr    -> (toStakeholders genesisListRich, toVss genesisListRich)
                TestnetCustomStakeDistr{..} -> (getGenesisWStakeholders tcsdBootStakeholders, tcsdVssCerts)

    logInfo $ sformat ("testnet genesis created successfully. "
                      %"First 10 addresses: "%listJson%" distr: "%shown)
              (map (sformat addressDetailedF) $ take 10 genesisAddrs)
              distr

    return (genesisAddrDistr, GenesisWStakeholders bootStakeholders, GenesisGtData gtData)

genFakeAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => Maybe FilePath -> FakeAvvmOptions -> m [AddrDistribution]
genFakeAvvmGenesis dirMB FakeAvvmOptions{..} = do
    fakeAvvmPubkeys <- case dirMB of
        Just dir -> do
            let keysDir = dir </> "keys-fakeavvm"
            logInfo $ "Generating fake avvm data into " <> fromString keysDir
            liftIO $ createDirectoryIfMissing True keysDir

            res <- forM [1 .. faoCount] $
                generateFakeAvvm . Just . (\x -> keysDir </> ("fake-" <> show x <> ".seed"))

            res <$ logInfo (show faoCount <> " fake avvm seeds are generated")
        Nothing -> replicateM (fromIntegral faoCount) (generateFakeAvvm Nothing)

    let gcdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gcdDistribution = CustomBalances $
            replicate (length gcdAddresses)
                      (mkCoin $ fromIntegral faoOneBalance)

    pure [(gcdAddresses, gcdDistribution)]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

generateKeyfile
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => Bool
    -> Maybe (SecretKey, EncryptedSecretKey)
    -> Maybe FilePath
    -> m (PublicKey, VssCertificate, Address)
generateKeyfile = generateKeyfile' (IsBootstrapEraAddr True)

generateKeyfile'
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => IsBootstrapEraAddr                     -- ^ is boot era
    -> Bool
    -> Maybe (SecretKey, EncryptedSecretKey)  -- ^ plain key & hd wallet root key
    -> Maybe FilePath
    -> m (PublicKey, VssCertificate, Address)  -- ^ plain key, certificate & hd wallet
                                               -- account address with bootstrap era distribution
generateKeyfile' isBoot isPrim mbSk fpMB = do
    (sk, hdwSk) <- case mbSk of
        Just x  -> return x
        Nothing -> liftIO $ runGlobalRandom $
            (,) <$> (snd <$> keyGen)
                <*> (snd <$> safeKeyGen emptyPassphrase)
    vss <- liftIO $ runGlobalRandom vssKeyGen

    whenJust fpMB $ \fp -> do
        initializeUserSecret fp
        us <- takeUserSecret fp

        writeUserSecretRelease $
            us & (if isPrim
                then usPrimKey .~ Just sk
                else (usKeys %~ (noPassEncrypt sk :))
                    . (usWalletSet ?~ mkGenesisWalletUserSecret hdwSk))
            & usVss .~ Just vss

    expiry <- liftIO $ runGlobalRandom $
        fromInteger <$>
        randomNumberInRange (Const.vssMinTTL - 1) (Const.vssMaxTTL - 1)
    let vssPk = asBinary $ toVssPublicKey vss
        vssCert = mkVssCertificate sk vssPk expiry
        -- This address is used only to create genesis data. We don't
        -- put it into a keyfile.
        hdwAccountPk =
            fst $ fromMaybe (error "generateKeyfile: pass mismatch") $
            deriveLvl2KeyPair isBoot emptyPassphrase hdwSk
                accountGenesisIndex wAddressGenesisIndex
    pure (toPublic sk, vssCert, hdwAccountPk)

generateFakeAvvm :: MonadIO m => Maybe FilePath -> m RedeemPublicKey
generateFakeAvvm fpMB = do
    seed <- liftIO $ runGlobalRandom $ getRandomBytes 32
    let (pk, _) = fromMaybe
            (error "Impossible - seed is not 32 bytes long") $
            redeemDeterministicKeyGen seed
    whenJust fpMB $ flip writeFile (B64.encode seed)
    pure pk

-- | Generates balance distribution for testnet.
genTestnetDistribution :: TestnetBalanceOptions -> BalanceDistribution
genTestnetDistribution TestnetBalanceOptions{..} =
    checkConsistency $ RichPoorBalances {..}
  where
    richs = fromIntegral tboRichmen
    poors = fromIntegral tboPoors * 2  -- for plain and hd wallet keys
    testBalance = fromIntegral tboTotalBalance

    -- Calculate actual balances
    desiredRichBalance = getShare tboRichmenShare testBalance
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
