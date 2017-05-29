{-# LANGUAGE ScopedTypeVariables #-}

module Main
       ( main
       ) where

import           Control.Lens         (each, _head)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Formatting           (sformat, shown, (%))
import           Options.Applicative  (execParser)
import           Serokell.Util.Text   (listJson)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory, (</>))
import           System.FilePath.Glob (glob)
import           System.Wlog          (WithLogger, usingLoggerName)
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Core             (coinToInteger, mkCoin, unsafeAddCoin,
                                       unsafeIntegerToCoin)
import           Pos.Genesis          (GenesisData (..), GenesisGtData (..),
                                       StakeDistribution (..), genesisDevHdwSecretKeys,
                                       genesisDevHdwSecretKeys, genesisDevSecretKeys,
                                       getTotalStake)
import           Pos.Types            (addressDetailedF, addressHash, makePubKeyAddress,
                                       makeRedeemAddress)

import           Avvm                 (aeCoin, applyBlacklisted, genGenesis, getHolderId,
                                       utxo)
import           KeygenOptions        (AvvmStakeOptions (..), FakeAvvmOptions (..),
                                       KeygenOptions (..), TestStakeOptions (..),
                                       optsInfo)
import           Testnet              (genTestnetStakes, generateFakeAvvm,
                                       generateKeyfile, rearrangeKeyfile)

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = toString . (T.replace `on` toText) a b . toText

applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp

getTestnetGenesis
    :: (MonadIO m, MonadFail m, WithLogger m)
    => TestStakeOptions -> m (GenesisData, GenesisGtData)
getTestnetGenesis tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    liftIO $ createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True Nothing (applyPattern tsoPattern i <> ".primary")
    poorsList <- forM [1 .. tsoPoors] $ \i ->
        generateKeyfile False Nothing (applyPattern tsoPattern i)

    let genesisList = richmenList ++ poorsList <&> \(k, vc, _) -> (k, vc)

    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes tso
        richmanStake = case distr of
            RichPoorStakes {..} -> sdRichStake
            _ -> error "cardano-keygen: impossible type of generated testnet stake"
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
                    <> map (view _3) poorsList
        genData = GenesisData
            { gdAddresses = genesisAddrs
            , gdDistribution = distr
            , gdBootstrapBalances = HM.fromList $
                map ((, richmanStake) . addressHash . fst) $
                take (fromIntegral tsoRichmen) genesisList
            }
        genGtData = GenesisGtData
            { ggdVssCertificates = HM.fromList $
                map (_1 %~ addressHash) $
                take (fromIntegral tsoRichmen) genesisList

            }

    putText $ "Total testnet genesis stake: " <> show distr
    return (genData, genGtData)

getFakeAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => FakeAvvmOptions -> m (GenesisData, GenesisGtData)
getFakeAvvmGenesis FakeAvvmOptions{..} = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory faoSeedPattern

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . applyPattern faoSeedPattern

    putText $ show faoCount <> " fake avvm seeds are generated"

    let gdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gdDistribution = ExplicitStakes $ HM.fromList $
            map (, (mkCoin $ fromIntegral faoOneStake, [])) gdAddresses
        gdBootstrapBalances = mempty
        ggdVssCertificates = mempty

    return (GenesisData{..}, GenesisGtData{..})

getAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => AvvmStakeOptions -> m (GenesisData, GenesisGtData)
getAvvmGenesis AvvmStakeOptions {..} = do
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    holder <- getHolderId asoHolderKeyfile
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- liftIO $ applyBlacklisted asoBlacklisted avvmData
            let totalAvvmStake = sum $ map aeCoin $ utxo avvmDataFiltered
            putText $ "Total avvm stake after applying blacklist: " <> show totalAvvmStake
            pure $ genGenesis avvmDataFiltered asoIsRandcerts holder

main :: IO ()
main = do
    ko@(KeygenOptions{..}) <- execParser optsInfo
    usingLoggerName "keygen" $
        if | Just msk <- koRearrangeMask  -> rearrange msk
           | Just pat <- koDumpDevGenKeys -> dumpKeys pat
           | otherwise                    -> genGenesisFiles ko

rearrange :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

dumpKeys :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
dumpKeys pat = do
    let keysDir = takeDirectory pat
    liftIO $ createDirectoryIfMissing True keysDir
    for_ (zip3 [1 ..] genesisDevSecretKeys genesisDevHdwSecretKeys) $
        \(i :: Int, k, wk) ->
        generateKeyfile False (Just (k, wk)) $ applyPattern pat i

reassignBalances :: GenesisData -> GenesisData
reassignBalances GenesisData{..} = GenesisData
    { gdBootstrapBalances = newBalances
    , ..
    }
  where
    newBalances = HM.fromList $ HM.toList gdBootstrapBalances
                  & each . _2 .~ newBalance
                  & _head . _2 .~ newBalance `unsafeAddCoin` remainder
    totalBalance = coinToInteger $ getTotalStake gdDistribution
    nBalances = fromIntegral $ length gdBootstrapBalances
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
                      (map (sformat addressDetailedF) . take 10 $ gdAddresses tg)
                      (gdDistribution tg)

    let mGenData = mappend <$> mTestnetGenesis <*> mAvvmGenesis
                   <|> mTestnetGenesis
                   <|> mAvvmGenesis
        genData' = fromMaybe (error "At least one of options \
                                    \(AVVM stake or testnet stake) \
                                    \should be provided") mGenData
        (reassignBalances -> genGeneralData, genGtData) =
            genData' <> fromMaybe mempty mFakeAvvmGenesis

    -- write genesis-general.bin
    do let bin = encode genGeneralData
           name = "genesis-general.bin"
           path = koGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisData) -> do
               liftIO $ BSL.writeFile path bin
               putText (toText name <> " generated successfully")
           Left err ->
               putText ("Generated GenesisData can't be read: " <> toText err)

    -- write godtossing/genesis-gt.bin
    do let bin = encode genGtData
           name = "godtossing" </> "genesis-gt.bin"
           path = koGenesisDir </> name
       liftIO $ createDirectoryIfMissing True (takeDirectory path)
       case decodeFull bin of
           Right (_ :: GenesisGtData) -> do
               liftIO $ BSL.writeFile path bin
               putText (toText name <> " generated successfully")
           Left err ->
               putText ("Generated GenesisGtData can't be read: " <> toText err)
