{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens         (each, _head)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Formatting           (sformat, shown, (%))
import           Options.Applicative  (execParser)
import           Serokell.Util.Text   (listJson)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)
import           System.FilePath.Glob (glob)
import           System.Wlog          (WithLogger, usingLoggerName)
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Core             (coinToInteger, mkCoin, unsafeAddCoin,
                                       unsafeIntegerToCoin)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..),
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
                                       generateHdwKeyfile, generateKeyfile,
                                       rearrangeKeyfile)

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = toString . (T.replace `on` toText) a b . toText

applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp

getTestnetGenesis
    :: (MonadIO m, MonadFail m, WithLogger m)
    => TestStakeOptions -> m GenesisData
getTestnetGenesis tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    liftIO $ createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True Nothing (applyPattern tsoPattern i <> ".primary")
    poorsList <- forM [1 .. tsoPoors] $ \i ->
        generateKeyfile False Nothing (applyPattern tsoPattern i)

    let genesisList = richmenList ++ poorsList

    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes tso
        richmanStake = case distr of
            RichPoorStakes {..} -> sdRichStake
            _ -> error "cardano-keygen: impossible type of generated testnet stake"
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
        genesisVssCerts = HM.fromList
                          $ map (_1 %~ addressHash)
                          $ take (fromIntegral tsoRichmen) genesisList
        genData = GenesisData
            { gdAddresses = genesisAddrs
            , gdDistribution = distr
            , gdVssCertificates = genesisVssCerts
            , gdBootstrapBalances = HM.fromList $
                map ((, richmanStake) . addressHash . fst) $
                genericTake tsoRichmen genesisList
            }

    putText $ "Total testnet genesis stake: " <> show distr
    return genData

getFakeAvvmGenesis :: (MonadIO m, WithLogger m) => FakeAvvmOptions -> m GenesisData
getFakeAvvmGenesis FakeAvvmOptions{..} = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory faoSeedPattern

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . applyPattern faoSeedPattern

    putText $ show faoCount <> " fake avvm seeds are generated"

    let gdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gdDistribution = ExplicitStakes $ HM.fromList $
            map (, (mkCoin $ fromIntegral faoOneStake, [])) gdAddresses
        gdVssCertificates = mempty
        gdBootstrapBalances = mempty

    return GenesisData {..}

getAvvmGenesis :: (MonadIO m, WithLogger m) => AvvmStakeOptions -> m GenesisData
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
           | otherwise                    -> genGenesisBin ko

rearrange :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

dumpKeys :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
dumpKeys pat = do
    let keysDir = takeDirectory pat
    liftIO $ createDirectoryIfMissing True keysDir
    for_ (zip [1..] genesisDevSecretKeys) $ \(i :: Int, k) ->
        generateKeyfile False (Just k) $ applyPattern pat i
    for_ (zip [1..] genesisDevHdwSecretKeys) $ \(i :: Int, k) ->
        generateHdwKeyfile k $ applyPattern (pat <> ".hd") i

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

genGenesisBin
    :: (MonadIO m, MonadFail m, WithLogger m)
    => KeygenOptions -> m ()
genGenesisBin KeygenOptions{..} = do
    let genFileDir = takeDirectory koGenesisFile
    liftIO $ createDirectoryIfMissing True genFileDir

    mAvvmGenesis <- traverse getAvvmGenesis koAvvmStake
    mTestnetGenesis <- traverse getTestnetGenesis koTestStake
    mFakeAvvmGenesis <- traverse getFakeAvvmGenesis koFakeAvvmStake
    whenJust mTestnetGenesis $ \tg ->
        putText $ sformat ("testnet genesis created successfully. "
                          %"First 30 addresses: "%listJson%" distr: "%shown)
                      (map (sformat addressDetailedF) . take 10 $ gdAddresses tg)
                      (gdDistribution <$> mTestnetGenesis)

    let mGenData = mappend <$> mTestnetGenesis <*> mAvvmGenesis
                   <|> mTestnetGenesis
                   <|> mAvvmGenesis
        genData' = fromMaybe (error "At least one of options \
                                    \(AVVM stake or testnet stake) \
                                    \should be provided") mGenData
        genData = reassignBalances $ genData' <> fromMaybe mempty mFakeAvvmGenesis
        binGenesis = encode genData

    case decodeFull binGenesis of
        Right (_ :: GenesisData) -> do
            putText "genesis.bin generated successfully\n"
            liftIO $ BSL.writeFile koGenesisFile binGenesis
        Left err                 -> do
            putText ("Generated genesis.bin can't be read: " <>
                     toText err <> "\n")
            if length binGenesis < 10*1024
                then putText "Printing GenesisData:\n\n" >> print genData
                else putText "genesis.bin is bigger than 10k, won't print it\n"
