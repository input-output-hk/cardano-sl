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
import           Pos.Genesis          (GenesisData (..))
import           Pos.Types            (addressDetailedF, addressHash, makePubKeyAddress)

import           Avvm                 (aeCoin, applyBlacklisted, genGenesis, getHolderId,
                                       utxo)
import           KeygenOptions        (AvvmStakeOptions (..), KeygenOptions (..),
                                       TestStakeOptions (..), optsInfo)
import           Testnet              (genTestnetStakes, generateKeyfile,
                                       rearrangeKeyfile)

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = toString . (T.replace `on` toText) a b . toText

getTestnetGenesis :: TestStakeOptions -> IO GenesisData
getTestnetGenesis tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors
        getFilename i = replace "{}" (show i) tsoPattern

    richmenList <- forM [1 .. tsoRichmen] $ \i ->
        generateKeyfile True $ getFilename i <> ".primary"
    poorsList <- forM [1 .. tsoPoors] $
        generateKeyfile False . getFilename

    let genesisList = richmenList ++ poorsList

    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes tso
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
        genesisVssCerts = HM.fromList
                          $ map (_1 %~ addressHash)
                          $ take (fromIntegral tsoRichmen) genesisList
        genData = GenesisData
            { gdAddresses = genesisAddrs
            , gdDistribution = distr
            , gdVssCertificates = genesisVssCerts
            }

    putText $ "Total testnet genesis stake: " <> show distr
    return genData

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
            whenJust mTestnetGenesis $ \tg ->
                putText $ sformat ("testnet genesis created successfully. First 30 addresses: "%listJson%" distr: "%shown)
                              (map (sformat addressDetailedF) . take 10 $ gdAddresses tg)
                              (gdDistribution <$> mTestnetGenesis)

            let mGenData = mappend <$> mTestnetGenesis <*> mAvvmGenesis
                           <|> mTestnetGenesis
                           <|> mAvvmGenesis
                genData = fromMaybe (error "At least one of options \
                                           \(AVVM stake or testnet stake) \
                                           \should be provided") mGenData
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
