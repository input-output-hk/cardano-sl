{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Core             (mkCoin)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..))
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

getTestnetGenesis :: TestStakeOptions -> IO GenesisData
getTestnetGenesis tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True $ applyPattern tsoPattern i <> ".primary"
    poorsList <- forM [1 .. tsoPoors] $
        generateKeyfile False . applyPattern tsoPattern

    let genesisList = richmenList ++ poorsList

    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes tso
        richmanStake = case distr of
            TestnetStakes {..} -> sdRichStake
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

getFakeAvvmGenesis :: FakeAvvmOptions -> IO GenesisData
getFakeAvvmGenesis FakeAvvmOptions{..} = do
    createDirectoryIfMissing True $ takeDirectory faoSeedPattern

    fakeAvvmPubkeys <- forM [1 .. faoCount] $
        generateFakeAvvm . applyPattern faoSeedPattern

    putText $ show faoCount <> " fake avvm seeds are generated"

    let gdAddresses = map makeRedeemAddress fakeAvvmPubkeys
        gdDistribution = ExplicitStakes $ HM.fromList $
            map (, (mkCoin $ fromIntegral faoOneStake, [])) gdAddresses
        gdVssCertificates = mempty
        gdBootstrapBalances = mempty

    return GenesisData {..}

getAvvmGenesis :: AvvmStakeOptions -> IO GenesisData
getAvvmGenesis AvvmStakeOptions {..} = do
    jsonfile <- BSL.readFile asoJsonPath
    holder <- getHolderId asoHolderKeyfile
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- applyBlacklisted asoBlacklisted avvmData
            let totalAvvmStake = sum $ map aeCoin $ utxo avvmDataFiltered
            putText $ "Total avvm stake after applying blacklist: " <> show totalAvvmStake
            pure $ genGenesis avvmDataFiltered asoIsRandcerts holder

main :: IO ()
main = do
    KeygenOptions {..} <- execParser optsInfo

    case koRearrangeMask of
        Just msk -> glob msk >>= mapM_ rearrangeKeyfile
        Nothing -> do
            let genFileDir = takeDirectory koGenesisFile
            createDirectoryIfMissing True genFileDir

            mAvvmGenesis <- traverse getAvvmGenesis koAvvmStake
            mTestnetGenesis <- traverse getTestnetGenesis koTestStake
            mFakeAvvmGenesis <- traverse getFakeAvvmGenesis koFakeAvvmStake
            whenJust mTestnetGenesis $ \tg ->
                putText $ sformat ("testnet genesis created successfully. First 30 addresses: "%listJson%" distr: "%shown)
                              (map (sformat addressDetailedF) . take 10 $ gdAddresses tg)
                              (gdDistribution <$> mTestnetGenesis)

            let mGenData = mappend <$> mTestnetGenesis <*> mAvvmGenesis
                           <|> mTestnetGenesis
                           <|> mAvvmGenesis
                genData' = fromMaybe (error "At least one of options \
                                            \(AVVM stake or testnet stake) \
                                            \should be provided") mGenData
                genData = genData' <> fromMaybe mempty mFakeAvvmGenesis
                binGenesis = encode genData

            case decodeFull binGenesis of
                Right (_ :: GenesisData) -> do
                    putText "genesis.bin generated successfully\n"
                    BSL.writeFile koGenesisFile binGenesis
                Left err                 -> do
                    putText ("Generated genesis.bin can't be read: " <>
                             toText err <> "\n")
                    if length binGenesis < 10*1024
                        then putText "Printing GenesisData:\n\n" >> print genData
                        else putText "genesis.bin is bigger than 10k, won't print it\n"
