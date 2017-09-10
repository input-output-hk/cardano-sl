module Main
       ( main
       ) where

import           Universum

import           Control.Lens               ((?~))
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.List                  as L
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Formatting                 (build, sformat, (%))
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory, (</>))
import           System.FilePath.Glob       (glob)
import           System.Wlog                (Severity (Debug), WithLogger, consoleOutB,
                                             lcTermSeverity, logError, logInfo,
                                             setupLogging, usingLoggerName)
import           Text.JSON.Canonical        (parseCanonicalJSON, prettyCanonicalJSON,
                                             toJSON)

import           Pos.Binary                 (asBinary)
import           Pos.Core                   (addressHash)
import           Pos.Crypto                 (EncryptedSecretKey (..), VssKeyPair,
                                             redeemPkB64F, toVssPublicKey)
import           Pos.Crypto.Signing         (SecretKey (..), toPublic)
import           Pos.Genesis                (AddrDistribution, GenesisSpec (..),
                                             genesisDevHdwSecretKeys,
                                             genesisDevSecretKeys, mkGenesisCoreData,
                                             noGenesisDelegation)
import           Pos.Util.UserSecret        (readUserSecret, usKeys, usPrimKey, usVss,
                                             usWalletSet)
import           Pos.Util.Util              (leftToPanic)
import           Pos.Wallet.Web.Secret      (wusRootKey)

import           Avvm                       (aeCoin, applyBlacklisted,
                                             avvmAddrDistribution, utxo)
import           KeygenOptions              (AvvmBalanceOptions (..),
                                             DumpAvvmSeedsOptions (..),
                                             GenesisGenOptions (..), KeygenCommand (..),
                                             KeygenOptions (..), getKeygenOptions)
import           Testnet                    (generateFakeAvvm, generateKeyfile,
                                             rearrangeKeyfile)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText

----------------------------------------------------------------------------
-- Balance distributions generation
----------------------------------------------------------------------------

-- Generates keys and vss certs for testnet data. Returns:
-- 1. Address distribution
-- 2. Set of boot stakeholders (richmen addresses)
-- 3. Genesis vss data (vss certs of richmen)
-- getTestnetData ::
--        (MonadIO m, MonadFail m, WithLogger m)
--     => FilePath
--     -> TestBalanceOptions
--     -> m ([AddrDistribution], Map StakeholderId Word16, GenesisGtData)
-- getTestnetData dir tso@TestBalanceOptions{..} = do

--     let keysDir = dir </> "keys-testnet"
--     let richDir = keysDir </> "rich"
--     let poorDir = keysDir </> "poor"
--     logInfo $ "Generating testnet data into " <> fromString keysDir
--     liftIO $ createDirectoryIfMissing True richDir
--     liftIO $ createDirectoryIfMissing True poorDir

--     let totalStakeholders = tsoRichmen + tsoPoors

--     richmenList <- forM [1 .. tsoRichmen] $ \i ->
--         generateKeyfile True Nothing $
--         richDir </> (applyPattern tsoPattern i)
--     poorsList <- forM [1 .. tsoPoors] $ \i ->
--         generateKeyfile False Nothing $
--         poorDir </> applyPattern tsoPattern i

--     let genesisList = map (\(k, vc, _) -> (k, vc)) $ richmenList ++ poorsList
--     let genesisListRich = take (fromIntegral tsoRichmen) genesisList

--     logInfo $ show totalStakeholders <> " keyfiles are generated"

--     let distr = genTestnetDistribution tso
--         richmenStakeholders = case distr of
--             RichPoorBalances {..} ->
--                 Map.fromList $ map ((,1) . addressHash . fst) genesisListRich
--             _ -> error "cardano-keygen: impossible type of generated testnet balance"
--         genesisAddrs = map (makePubKeyAddressBoot . fst) genesisList
--                     <> map (view _3) poorsList
--         genesisAddrDistr = [(genesisAddrs, distr)]
--         genGtData = GenesisGtData
--             { ggdVssCertificates =
--               HM.fromList $ map (_1 %~ addressHash) genesisListRich
--             }

--     logInfo $ sformat ("testnet genesis created successfully. "
--                       %"First 10 addresses: "%listJson%" distr: "%shown)
--               (map (sformat addressDetailedF) $ take 10 genesisAddrs)
--               distr

--     return (genesisAddrDistr, richmenStakeholders, genGtData)

-- getFakeAvvmGenesis
--     :: (MonadIO m, WithLogger m)
--     => FilePath -> FakeAvvmOptions -> m [AddrDistribution]
-- getFakeAvvmGenesis dir FakeAvvmOptions{..} = do
--     let keysDir = dir </> "keys-fakeavvm"
--     logInfo $ "Generating fake avvm data into " <> fromString keysDir
--     liftIO $ createDirectoryIfMissing True keysDir

--     fakeAvvmPubkeys <- forM [1 .. faoCount] $
--         generateFakeAvvm . (\x -> keysDir </> ("fake-"<>show x<>".seed"))

--     logInfo $ show faoCount <> " fake avvm seeds are generated"

--     let gcdAddresses = map makeRedeemAddress fakeAvvmPubkeys
--         gcdDistribution = CustomBalances $
--             replicate (length gcdAddresses)
--                       (mkCoin $ fromIntegral faoOneBalance)

--     pure $ [(gcdAddresses, gcdDistribution)]

-- Reads avvm json file and returns related 'AddrDistribution'
getAvvmGenesis
    :: (MonadIO m, WithLogger m, MonadFail m)
    => AvvmBalanceOptions -> m AddrDistribution
getAvvmGenesis AvvmBalanceOptions {..} = do
    logInfo "Generating avvm data"
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- applyBlacklisted asoBlacklisted avvmData
            let totalAvvmBalance = sum $ map aeCoin $ utxo avvmDataFiltered
            logInfo $ "Total avvm balance after applying blacklist: " <> show totalAvvmBalance
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
    when (isNothing ggoAvvmBalance &&
          isNothing ggoTestBalance &&
          isNothing ggoFakeAvvmBalance) $
        error "At least one of options (AVVM balance or testnet balance) \
              \should be provided"

    logInfo "Generating AVVM distribution"
    mAvvmAddrDistr <- traverse getAvvmGenesis ggoAvvmBalance

    ------ Generating genesis JSON data

    -- avvm address distribution
    let gcdAvvmAddrDistribution = maybe [] one mAvvmAddrDistr

    -- [CSL-1596] TODO: add CLI for it!
    let gcdHeavyDelegation = noGenesisDelegation
    let avvmGenCoreData =
            leftToPanic "Couldn't create genesis core data: " $
                mkGenesisCoreData
                    gcdAvvmAddrDistribution
                    (Map.fromList ggoBootStakeholders)
                    gcdHeavyDelegation
    let genSpec =
            GenesisSpec
                avvmGenCoreData
                ggoTestBalance
                ggoFakeAvvmBalance
                ggoSeed

    ------ Writing/dumping

    -- write genesis.json
    prettyJSON <- prettyCanonicalJSON <$> toJSON genSpec
    let name = "genesis.json"
    let path = ggoGenesisDir </> name
    liftIO $ createDirectoryIfMissing True (takeDirectory path)
    case parseCanonicalJSON (BSLC.pack prettyJSON) of
        Right _ -> do
            liftIO $ BSL.writeFile path $ BSLC.pack prettyJSON
            logInfo (toText name <> " generated successfully")
        Left err ->
            logError ("Generated GenesisCoreData can't be read: " <> toText err)

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
