{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Options.Applicative  (execParser)
import           Prelude              (show)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)
import           System.Random        (randomRIO)
import           Universum            hiding (show)

import           Pos.Binary           (asBinary, decodeFull, encode)
import           Pos.Constants        (vssMaxTTL, vssMinTTL)
import           Pos.Crypto           (PublicKey, keyGen, toPublic, toVssPublicKey,
                                       vssKeyGen)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..),
                                       getTotalStake)
import           Pos.Ssc.GodTossing   (VssCertificate, mkVssCertificate)
import           Pos.Types            (addressHash, makePubKeyAddress, mkCoin)
import           Pos.Util.UserSecret  (initializeUserSecret, takeUserSecret, usPrimKey,
                                       usVss, writeUserSecretRelease)

import           Avvm                 (genGenesis, getHolderId)
import           KeygenOptions        (AvvmStakeOptions (..), KeygenOptions (..),
                                       TestStakeOptions (..), optsInfo)

generateKeyfile :: FilePath -> IO (PublicKey, VssCertificate)
generateKeyfile fp = do
    initializeUserSecret fp
    sk <- snd <$> keyGen
    vss <- vssKeyGen
    us <- takeUserSecret fp
    writeUserSecretRelease $
        us & usPrimKey .~ Just sk
           & usVss .~ Just vss
    expiry <- fromIntegral <$> randomRIO (vssMinTTL :: Int, vssMaxTTL)
    let vssPk = asBinary $ toVssPublicKey vss
        vssCert = mkVssCertificate sk vssPk expiry
    return (toPublic sk, vssCert)

replace :: FilePath -> FilePath -> FilePath -> FilePath
replace a b = toString . (T.replace `on` toText) a b . toText

getTestnetGenesis :: TestStakeOptions -> IO GenesisData
getTestnetGenesis TSO{..} = do
    let keysDir = takeDirectory tsoPattern
    createDirectoryIfMissing True keysDir

    genesisList <- forM [1..tsoStakeholders] $ \i ->
        generateKeyfile $ replace "{}" (show i) tsoPattern
    print $ show tsoStakeholders ++ " keyfiles are generated"

    let distr = TestnetStakes
            { sdTotalStake = mkCoin tsoTotalStake
            , sdRichmen    = tsoRichmen
            , sdPoor       = tsoStakeholders - tsoRichmen
            }
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
getAvvmGenesis ASO{..} = do
    jsonfile <- BSL.readFile asoJsonPath
    holder <- getHolderId asoHolderKeyfile
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> pure $
            genGenesis avvmData asoIsRandcerts holder

main :: IO ()
main = do
    KO {..} <- execParser optsInfo
    let genFileDir = takeDirectory koGenesisFile
    createDirectoryIfMissing True genFileDir

    mTestnetGenesis <- traverse getTestnetGenesis koTestStake
    mAvvmGenesis <- traverse getAvvmGenesis koAvvmStake

    let mGenData = mappend <$> mTestnetGenesis <*> mAvvmGenesis
                   <|> mTestnetGenesis
                   <|> mAvvmGenesis
        genData = maybe (error "At least one of options \
                               \(AVVM stake or testnet stake) \
                               \should be provided")
                  identity mGenData
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
