
module Main
       ( main
       ) where

import           Universum

import           Control.Lens          ((?~))
import           Data.Aeson            (eitherDecode)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Formatting            (build, sformat, shown, (%))
import           Serokell.Util.Text    (listJson)
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory, (</>))
import           System.FilePath.Glob  (glob)
import           System.Wlog           (Severity (Debug), WithLogger, consoleOutB,
                                        lcTermSeverity, logError, logInfo, setupLogging,
                                        usingLoggerName)

import           Pos.Binary            (asBinary, decodeFull, serialize')
import           Pos.Core              (StakeholderId, addressDetailedF, addressHash,
                                        makePubKeyAddressBoot, makeRedeemAddress, mkCoin)
import           Pos.Crypto            (EncryptedSecretKey (..), VssKeyPair, redeemPkB64F,
                                        toVssPublicKey)
import           Pos.Crypto.Signing    (SecretKey (..), toPublic)
import           Pos.Genesis           (AddrDistribution, GenesisCoreData (..),
                                        GenesisGtData (..), StakeDistribution (..),
                                        genesisDevHdwSecretKeys, genesisDevSecretKeys,
                                        mkGenesisCoreData, noGenesisDelegation)
import           Pos.Util.UserSecret   (readUserSecret, usKeys, usPrimKey, usVss,
                                        usWalletSet)
import           Pos.Util.Util         (leftToPanic)
import           Pos.Wallet.Web.Secret (wusRootKey)

import           Avvm                  (aeCoin, applyBlacklisted, avvmAddrDistribution,
                                        utxo)
import           KeygenOptions         (AvvmStakeOptions (..), DumpAvvmSeedsOptions (..),
                                        FakeAvvmOptions (..), GenesisGenOptions (..),
                                        KeygenCommand (..), KeygenOptions (..),
                                        TestStakeOptions (..), getKeygenOptions)
import           Testnet               (genTestnetDistribution, generateFakeAvvm,
                                        generateKeyfile, rearrangeKeyfile)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText

----------------------------------------------------------------------------
-- Stake distributions generation
----------------------------------------------------------------------------

-- Generates keys and vss certs for testnet data. Returns:
-- 1. Address distribution
-- 2. Set of boot stakeholders (richmen addresses)
-- 3. Genesis vss data (vss certs of richmen)
getTestnetData ::
       (MonadIO m, MonadFail m, WithLogger m)
    => FilePath
    -> TestStakeOptions
    -> m ([AddrDistribution], Map StakeholderId Word16, GenesisGtData)
getTestnetData dir tso@TestStakeOptions{..} = do

    let keysDir = dir </> "keys-testnet"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo $ "Generating testnet data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True richDir
    liftIO $ createDirectoryIfMissing True poorDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True Nothing $
        richDir </> (applyPattern tsoPattern i)
    poorsList <- forM [1 .. tsoPoors] $ \i ->
        generateKeyfile False Nothing $
        poorDir </> applyPattern tsoPattern i

    let genesisList = map (\(k, vc, _) -> (k, vc)) $ richmenList ++ poorsList
    let genesisListRich = take (fromIntegral tsoRichmen) genesisList

    logInfo $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetDistribution tso
        richmenStakeholders = case distr of
            RichPoorStakes {..} ->
                Map.fromList $ map ((,1) . addressHash . fst) genesisListRich
            _ -> error "cardano-keygen: impossible type of generated testnet stake"
        genesisAddrs = map (makePubKeyAddressBoot . fst) genesisList
                    <> map (view _3) poorsList
        genesisAddrDistr = [(genesisAddrs, distr)]
        genGtData = GenesisGtData
            { ggdVssCertificates =
              HM.fromList $ map (_1 %~ addressHash) genesisListRich
            }

    logInfo $ sformat ("testnet genesis created successfully. "
                      %"First 10 addresses: "%listJson%" distr: "%shown)
              (map (sformat addressDetailedF) $ take 10 genesisAddrs)
              distr

    return (genesisAddrDistr, richmenStakeholders, genGtData)

getFakeAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => FilePath -> FakeAvvmOptions -> m [AddrDistribution]
getFakeAvvmGenesis dir FakeAvvmOptions{..} = do
    let keysDir = dir </> "keys-fakeavvm"
    logInfo $ "Generating fake avvm data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True keysDir

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . (\x -> keysDir </> ("fake-"<>show x<>".seed"))

    logInfo $ show faoCount <> " fake avvm seeds are generated"

    let gcdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gcdDistribution = CustomStakes $
            replicate (length gcdAddresses)
                      (mkCoin $ fromIntegral faoOneStake)

    pure $ [(gcdAddresses, gcdDistribution)]

-- Reads avvm json file and returns related 'AddrDistribution'
getAvvmGenesis
    :: (MonadIO m, WithLogger m, MonadFail m)
    => AvvmStakeOptions -> m [AddrDistribution]
getAvvmGenesis AvvmStakeOptions {..} = do
    logInfo "Generating avvm data"
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- applyBlacklisted asoBlacklisted avvmData
            let totalAvvmStake = sum $ map aeCoin $ utxo avvmDataFiltered
            logInfo $ "Total avvm stake after applying blacklist: " <> show totalAvvmStake
            pure $ avvmAddrDistribution avvmDataFiltered

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

genPrimaryKey :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
genPrimaryKey path = do
    _ <- generateKeyfile True Nothing path
    logInfo $ "Successfully generated primary key " <> (toText path)

readKey :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
readKey path = do
    us <- readUserSecret path
    logInfo $ maybe "No Pimary key"
                    (("Primary: " <>) . showKeyWithAddressHash) $
                    view usPrimKey us
    logInfo $ maybe "No wallet set"
                    (("Wallet set: " <>) . showKeyWithAddressHash . decryptESK . view wusRootKey) $
                    view usWalletSet us
    logInfo $ "Keys: " <> (T.concat $ L.intersperse "\n" $
                           map (showKeyWithAddressHash . decryptESK) $
                           view usKeys us)
    logInfo $ maybe "No vss"
                    (("Vss PK: " <>) . showPvssKey) $
                    view usVss us

showKeyWithAddressHash :: SecretKey -> Text
showKeyWithAddressHash sk = sformat (build%"; address hash: "%build) pk ah
  where
    pk = toPublic sk
    ah = addressHash pk

showPvssKey :: VssKeyPair -> Text
showPvssKey = sformat build . asBinary . toVssPublicKey

decryptESK :: EncryptedSecretKey -> SecretKey
decryptESK (EncryptedSecretKey sk _) = SecretKey sk

dumpKeys :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
dumpKeys pat = do
    let keysDir = takeDirectory pat
    liftIO $ createDirectoryIfMissing True keysDir
    for_ (zip3 [1 ..] genesisDevSecretKeys genesisDevHdwSecretKeys) $
        \(i :: Int, k, wk) ->
        generateKeyfile False (Just (k, wk)) $ applyPattern pat i

dumpAvvmSeeds
    :: (MonadIO m, WithLogger m)
    => DumpAvvmSeedsOptions -> m ()
dumpAvvmSeeds DumpAvvmSeedsOptions{..} = do
    logInfo $ "Generating fake avvm data into " <> fromString dasPath
    liftIO $ createDirectoryIfMissing True dasPath

    when (dasNumber <= 0) $ error $
        "number of seeds should be positive, but it's " <> show dasNumber

    fakeAvvmPubkeys <- forM [1 .. dasNumber] $
        generateFakeAvvm . (\x -> dasPath </> ("key"<>show x<>".seed"))
    forM_ (fakeAvvmPubkeys `zip` [1..dasNumber]) $
        \(rPk,i) -> writeFile (dasPath </> "key"<>show i<>".pk")
                              (sformat redeemPkB64F rPk)

    logInfo $ "Seeds were generated"

genGenesisFiles
    :: (MonadIO m, MonadFail m, WithLogger m)
    => GenesisGenOptions -> m ()
genGenesisFiles GenesisGenOptions{..} = do
    when (isNothing ggoAvvmStake &&
          isNothing ggoTestStake &&
          isNothing ggoFakeAvvmStake) $
        error "At least one of options (AVVM stake or testnet stake) \
              \should be provided"

    logInfo "Generating requested raw data"
    mAvvmAddrDistr <- traverse getAvvmGenesis ggoAvvmStake
    mFakeAvvmAddrDistr <- traverse (getFakeAvvmGenesis ggoGenesisDir) ggoFakeAvvmStake
    mTestnetData <- traverse (getTestnetData ggoGenesisDir) ggoTestStake

    ------ Generating genesis core data

    -- address distribution
    let gcdAddrDistribution =
            concat $ catMaybes
            [mAvvmAddrDistr, mFakeAvvmAddrDistr, view _1 <$> mTestnetData]
    when (null gcdAddrDistribution) $ error "gcdAddrDistribution is empty"
    -- boot stakeholders
    let gcdBootstrapStakeholders
            | null ggoBootStakeholders =
                mconcat $ catMaybes [ view _2 <$> mTestnetData ]
            | otherwise =
                Map.fromList ggoBootStakeholders
    when (null gcdBootstrapStakeholders) $
        error "gcdBootstrapStakeholders is empty. You can pass genesis \
              \stakeholders explicitly or use testnet genesis."
    -- [CSL-1596] TODO: add CLI for it!
    let genesisDelegation = noGenesisDelegation
    let genCoreData =
            leftToPanic "Couldn't create genesis core data: " $
                mkGenesisCoreData
                    gcdAddrDistribution
                    gcdBootstrapStakeholders
                    genesisDelegation

    ------ Generating GT core data

    let genGtData =
            mconcat $ catMaybes
            [ view _3 <$> mTestnetData
            -- , CSL-1315 vss certificates for boot stakeholders
            ]
    when (HM.null $ ggdVssCertificates genGtData) $
        error "genGtData seems to be empty. Are you sure you've specified testnet \
              \flag ? See CSL-1315"

    ------ Writing/dumping

    -- write genesis-core.bin
    do let bin = serialize' genCoreData
           name = "genesis-core.bin"
           path = ggoGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisCoreData) -> do
               liftIO $ BSL.writeFile path $ BSL.fromStrict bin
               logInfo (toText name <> " generated successfully")
           Left err ->
               logError ("Generated GenesisCoreData can't be read: " <> toText err)

    -- write godtossing/genesis-godtossing.bin
    do let bin = serialize' genGtData
           name = "genesis-godtossing.bin"
           path = ggoGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisGtData) -> do
               liftIO $ BSL.writeFile path $ BSL.fromStrict bin
               logInfo (toText name <> " generated successfully")
           Left err ->
               logError ("Generated GenesisGtData can't be read: " <> toText err)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    KeygenOptions{..} <- getKeygenOptions
    setupLogging $ consoleOutB & lcTermSeverity ?~ Debug
    usingLoggerName "keygen" $ do
        logInfo "Processing command"
        case koCommand of
            RearrangeMask msk   -> rearrange msk
            GenerateKey path    -> genPrimaryKey path
            ReadKey path        -> readKey path
            DumpDevGenKeys pat  -> dumpKeys pat
            DumpAvvmSeeds opts  -> dumpAvvmSeeds opts
            GenerateGenesis ggo -> genGenesisFiles ggo
