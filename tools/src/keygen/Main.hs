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
import           System.Wlog          (WithLogger, usingLoggerName)
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Core             (mkCoin)
import           Pos.Genesis          (GenesisCoreData (..), GenesisGtData (..),
                                       StakeDistribution (..), genesisDevHdwSecretKeys,
                                       genesisDevHdwSecretKeys, genesisDevSecretKeys)
import           Pos.Types            (addressDetailedF, addressHash, makePubKeyAddress,
                                       makeRedeemAddress)

import           Avvm                 (aeCoin, applyBlacklisted, genGenesis, utxo)
import           KeygenOptions        (AvvmStakeOptions (..), FakeAvvmOptions (..),
                                       KeygenOptions (..), TestStakeOptions (..),
                                       getKeygenOptions)
import           Testnet              (genTestnetStakes, generateFakeAvvm,
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

getTestnetGenesis
    :: (MonadIO m, MonadFail m, WithLogger m)
    => TestStakeOptions -> m (GenesisCoreData, GenesisGtData)
getTestnetGenesis tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    liftIO $ createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True Nothing (applyPattern tsoPattern i <> ".primary")
    poorsList <- forM [1 .. tsoPoors] $ \i ->
        generateKeyfile False Nothing (applyPattern tsoPattern i)

    let genesisList = map (\(k, vc, _) -> (k, vc)) $ richmenList ++ poorsList
    let genesisListRich = take (fromIntegral tsoRichmen) genesisList

    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes tso
        richmenStakeholders = case distr of
            RichPoorStakes {..} ->
                HS.fromList $ map (addressHash . fst) genesisListRich
            _ -> error "cardano-keygen: impossible type of generated testnet stake"
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
                    <> map (view _3) poorsList
        genData = GenesisCoreData
            { gcdAddresses = genesisAddrs
            , gcdDistribution = distr
            , gcdBootstrapStakeholders = richmenStakeholders
            }
        genGtData = GenesisGtData
            { ggdVssCertificates =
              HM.fromList $ map (_1 %~ addressHash) genesisListRich
            }

    putText $ "Total testnet genesis stake: " <> show distr
    return (genData, genGtData)

getFakeAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => FakeAvvmOptions -> m (GenesisCoreData, GenesisGtData)
getFakeAvvmGenesis FakeAvvmOptions{..} = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory faoSeedPattern

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . applyPattern faoSeedPattern

    putText $ show faoCount <> " fake avvm seeds are generated"

    let gcdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gcdDistribution = ExplicitStakes $ HM.fromList $
            map (, (mkCoin $ fromIntegral faoOneStake, [])) gcdAddresses
        gcdBootstrapStakeholders = mempty
        ggdVssCertificates = mempty

    return (GenesisCoreData{..}, GenesisGtData{..})

getAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => AvvmStakeOptions -> m (GenesisCoreData, GenesisGtData)
getAvvmGenesis AvvmStakeOptions {..} = do
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- liftIO $ applyBlacklisted asoBlacklisted avvmData
            let totalAvvmStake = sum $ map aeCoin $ utxo avvmDataFiltered
            putText $ "Total avvm stake after applying blacklist: " <> show totalAvvmStake
            -- warning: if testnet is not enabled,
            -- gcdBootstrapAddresses will be empty. We should come up
            -- with a way to pass real boot addresses to avvm
            -- distribution.
            pure $ genGenesis avvmDataFiltered asoIsRandcerts mempty

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

-- | Reassigns all balances in utxo to 'gcdBootstrapStakeholders'.
reassignBalances :: GenesisCoreData -> GenesisCoreData
reassignBalances GenesisCoreData{..} = GenesisCoreData
    { gcdBootstrapBalances = newBalances
    , ..
    }
  where
    newBalances = HM.fromList $ HM.toList gcdBootstrapBalances
                  & each . _2 .~ newBalance
                  & _head . _2 .~ newBalance `unsafeAddCoin` remainder
    totalBalance = coinToInteger $ getTotalStake gcdDistribution
    nBalances = fromIntegral $ length gcdBootstrapBalances
    newBalance = unsafeIntegerToCoin $ totalBalance `div` nBalances
    remainder = unsafeIntegerToCoin $ totalBalance `mod` nBalances


genGenesisFiles
    :: (MonadIO m, MonadFail m, WithLogger m)
    => KeygenOptions -> m ()
genGenesisFiles KeygenOptions{..} = do
    mAvvmGenesis <- traverse getAvvmGenesis koAvvmStake
    mTestnetGenesis <- traverse getTestnetGenesis koTestStake
    mFakeAvvmGenesis <- traverse getFakeAvvmGenesis koFakeAvvmStake
    whenJust mTestnetGenesis $ \(tg, _) ->
        putText $ sformat ("testnet genesis created successfully. "
                          %"First 30 addresses: "%listJson%" distr: "%shown)
                      (map (sformat addressDetailedF) . take 10 $ gcdAddresses tg)
                      (gcdDistribution tg)

    let atNoDistr = error "At least one of options (AVVM stake or testnet stake) \
                           \should be provided"
        mGenData0 = catMaybes [mTestnetGenesis, mAvvmGenesis]
        genData' = mconcat $ bool mGenData0 atNoDistr (null mGenData0)
        (genCoreData, genGtData) = genData' <> fromMaybe mempty mFakeAvvmGenesis

    -- write genesis-core.bin
    do let bin = encode genCoreData
           name = "core" </> "genesis-core.bin"
           path = koGenesisDir </> name
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
           path = koGenesisDir </> name
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
    ko@(KeygenOptions{..}) <- getKeygenOptions
    usingLoggerName "keygen" $
        if | Just msk <- koRearrangeMask  -> rearrange msk
           | Just pat <- koDumpDevGenKeys -> dumpKeys pat
           | otherwise                    -> genGenesisFiles ko
