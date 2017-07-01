{-# LANGUAGE ScopedTypeVariables #-}

module Main
       ( main
       ) where

import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.Text            as T
import           Formatting           (sformat, shown, (%))
import           Serokell.Util.Text   (listJson)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory, (</>))
import           System.FilePath.Glob (glob)
import           System.Wlog          (WithLogger, logInfo, usingLoggerName)
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Core             (StakeholderId, mkCoin)
import           Pos.Genesis          (AddrDistribution, GenesisCoreData (..),
                                       GenesisGtData (..), StakeDistribution (..),
                                       genesisDevHdwSecretKeys, genesisDevHdwSecretKeys,
                                       genesisDevSecretKeys, mkGenesisCoreData)
import           Pos.Types            (addressDetailedF, addressHash, makePubKeyAddress,
                                       makeRedeemAddress)

import           Avvm                 (aeCoin, applyBlacklisted, avvmAddrDistribution,
                                       utxo)
import           KeygenOptions        (AvvmStakeOptions (..), FakeAvvmOptions (..),
                                       GenesisGenOptions (..), KeygenOptions (..),
                                       TestStakeOptions (..), getKeygenOptions)
import           Testnet              (genTestnetDistribution, generateFakeAvvm,
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
    -> m (AddrDistribution, HashSet StakeholderId, GenesisGtData)
getTestnetData dir tso@TestStakeOptions{..} = do
    let keysDir = dir </> "keys-testnet"
    logInfo $ "Generating testnet data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True Nothing $
        keysDir </> (applyPattern tsoPattern i <> ".primary")
    poorsList <- forM [1 .. tsoPoors] $ \i ->
        generateKeyfile False Nothing $
        keysDir </> applyPattern tsoPattern i

    let genesisList = map (\(k, vc, _) -> (k, vc)) $ richmenList ++ poorsList
    let genesisListRich = take (fromIntegral tsoRichmen) genesisList

    logInfo $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetDistribution tso
        richmenStakeholders = case distr of
            RichPoorStakes {..} ->
                HS.fromList $ map (addressHash . fst) genesisListRich
            _ -> error "cardano-keygen: impossible type of generated testnet stake"
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
                    <> map (view _3) poorsList
        genesisAddrDistr = [(HS.fromList genesisAddrs, distr)]
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
    => FilePath -> FakeAvvmOptions -> m AddrDistribution
getFakeAvvmGenesis dir FakeAvvmOptions{..} = do
    let keysDir = dir </> "keys-fakeavvm"
    liftIO $ createDirectoryIfMissing True keysDir

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . (\x -> keysDir </> ("fake-"<>show x<>".seed"))

    putText $ show faoCount <> " fake avvm seeds are generated"

    let gcdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gcdDistribution = CustomStakes $
            replicate (length gcdAddresses)
                      (mkCoin $ fromIntegral faoOneStake)

    pure $ [(HS.fromList gcdAddresses, gcdDistribution)]

-- Reads avvm json file and returns related 'AddrDistribution'
getAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => AvvmStakeOptions -> m AddrDistribution
getAvvmGenesis AvvmStakeOptions {..} = do
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- liftIO $ applyBlacklisted asoBlacklisted avvmData
            let totalAvvmStake = sum $ map aeCoin $ utxo avvmDataFiltered
            putText $ "Total avvm stake after applying blacklist: " <> show totalAvvmStake
            pure $ avvmAddrDistribution avvmDataFiltered

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

dumpKeys :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
dumpKeys pat = do
    let keysDir = takeDirectory pat
    liftIO $ createDirectoryIfMissing True keysDir
    for_ (zip3 [1 ..] genesisDevSecretKeys genesisDevHdwSecretKeys) $
        \(i :: Int, k, wk) ->
        generateKeyfile False (Just (k, wk)) $ applyPattern pat i

genGenesisFiles
    :: (MonadIO m, MonadFail m, WithLogger m)
    => GenesisGenOptions -> m ()
genGenesisFiles GenesisGenOptions{..} = do
    when (isNothing ggoAvvmStake &&
          isNothing ggoTestStake &&
          isNothing ggoFakeAvvmStake) $
        error "At least one of options (AVVM stake or testnet stake) \
              \should be provided"

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
    let gcdBootstrapStakeholders =
            mconcat $ catMaybes
            [ view _2 <$> mTestnetData
            -- , CSL-1315 real boot stakeholders for mainnet, as addresses list
            ]
    when (null gcdBootstrapStakeholders) $
        error "gcdBootstrapStakeholders is empty. Current keygen implementation \
              \doesn't support explicit boot stakeholders, so if testnet is not \
              \enabled it can't work (with testnet case we take richmen as bootst.). \
              \See CSL-1315"
    let genCoreData =
            either (\e -> error $ "Couldn't create genesis core data: " <> fromString e)
                   identity
                   (mkGenesisCoreData gcdAddrDistribution gcdBootstrapStakeholders)

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
    do let bin = encode genCoreData
           name = "core" </> "genesis-core.bin"
           path = ggoGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisCoreData) -> do
               liftIO $ BSL.writeFile path $ BSL.fromStrict bin
               putText (toText name <> " generated successfully")
           Left err ->
               putText ("Generated GenesisCoreData can't be read: " <> toText err)

    -- write godtossing/genesis-godtossing.bin
    do let bin = encode genGtData
           name = "godtossing" </> "genesis-godtossing.bin"
           path = ggoGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisGtData) -> do
               liftIO $ BSL.writeFile path $ BSL.fromStrict bin
               putText (toText name <> " generated successfully")
           Left err ->
               putText ("Generated GenesisGtData can't be read: " <> toText err)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    KeygenOptions{..} <- getKeygenOptions
    usingLoggerName "keygen" $
        if | Just msk <- koRearrangeMask  -> rearrange msk
           | Just pat <- koDumpDevGenKeys -> dumpKeys pat
           | Just ggo <- koGenesisGen     -> genGenesisFiles ggo
           | otherwise                    -> error "No commands were specified!"
