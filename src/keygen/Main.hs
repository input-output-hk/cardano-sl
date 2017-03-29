{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Options.Applicative  (execParser)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)
import           Universum

import           Pos.Binary           (decodeFull, encode)
import           Pos.Genesis          (GenesisData (..), getTotalStake)
import           Pos.Types            (Coin, addressHash, makePubKeyAddress, mkCoin)

import           Avvm                 (genGenesis, getHolderId)
import           KeygenOptions        (AvvmStakeOptions (..), KeygenOptions (..),
                                       TestStakeOptions (..), optsInfo)
import           Testnet              (genTestnetStakes, generateKeyfile)

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = toString . (T.replace `on` toText) a b . toText

getTestnetGenesis :: Coin -> TestStakeOptions -> IO GenesisData
getTestnetGenesis avvmStake tso@TestStakeOptions{..} = do
    let keysDir = takeDirectory tsoPattern
    createDirectoryIfMissing True keysDir

    let totalStakeholders = tsoRichmen + tsoPoors
    genesisList <- forM [1 .. totalStakeholders] $ \i ->
        generateKeyfile $ replace "{}" (show i) tsoPattern
    putText $ show totalStakeholders <> " keyfiles are generated"

    let distr = genTestnetStakes avvmStake tso
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
        genesisVssCerts = HM.fromList
                          $ map (_1 %~ addressHash)
                          $ take (fromIntegral tsoRichmen) genesisList
        genData = GenesisData
            { gdAddresses = genesisAddrs
            , gdDistribution = distr
            , gdVssCertificates = genesisVssCerts
            }
    return genData

getAvvmGenesis :: AvvmStakeOptions -> IO GenesisData
getAvvmGenesis AvvmStakeOptions{..} = do
    jsonfile <- BSL.readFile asoJsonPath
    holder <- getHolderId asoHolderKeyfile
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> pure $
            genGenesis avvmData asoIsRandcerts holder

main :: IO ()
main = do
    KeygenOptions {..} <- execParser optsInfo
    let genFileDir = takeDirectory koGenesisFile
    createDirectoryIfMissing True genFileDir

    mAvvmGenesis <- traverse getAvvmGenesis koAvvmStake
    let avvmStake = maybe (mkCoin 0)
            (getTotalStake . gdDistribution) mAvvmGenesis
    mTestnetGenesis <- traverse (getTestnetGenesis avvmStake) koTestStake

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
