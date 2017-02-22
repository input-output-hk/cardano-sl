{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Options.Applicative  (Parser, ParserInfo, auto, execParser, fullDesc,
                                       help, helper, info, long, metavar, option, option,
                                       progDesc, short, strOption, value)
import           Prelude              (show)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)
import           System.Random        (randomRIO)
import           Universum            hiding (show)

import           Pos.Binary           (asBinary, decodeFull, encode)
import           Pos.Constants        (vssMaxTTL, vssMinTTL)
import           Pos.Crypto           (PublicKey, keyGen, toPublic, toVssPublicKey,
                                       vssKeyGen)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..))
import           Pos.Ssc.GodTossing   (VssCertificate, mkVssCertificate)
import           Pos.Types            (addressHash, makePubKeyAddress, mkCoin)
import           Pos.Util.UserSecret  (initializeUserSecret, takeUserSecret, usKeys,
                                       usPrimKey, usVss, writeUserSecretRelease)

data KeygenOptions = KO
    { koPattern      :: FilePath
    , koGenesisFile  :: FilePath
    , koStakeholders :: Word
    , koRichmen      :: Word
    , koTotalStake   :: Word64
    }

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
replace a b = T.unpack . (T.replace `on` T.pack) a b . T.pack

optsParser :: Parser KeygenOptions
optsParser = do
    koPattern <- strOption $
        long    "file-pattern" <>
        short   'f' <>
        metavar "PATTERN" <>
        help    "Filename pattern for generated keyfiles \
                \(`{}` is a place for number)"
    koGenesisFile <- strOption $
        long    "genesis-file" <>
        metavar "FILE" <>
        value   "genesis.bin" <>
        help    "File to dump binary shared genesis data"
    koStakeholders <- option auto $
        long    "total-stakeholders" <>
        short   'n' <>
        metavar "INT" <>
        help    "Total number of keyfiles to generate"
    koRichmen <- option auto $
        long    "richmen" <>
        short   'm' <>
        metavar "INT" <>
        help    "Number of richmen among stakeholders"
    koTotalStake <- option auto $
        long    "total-stake" <>
        metavar "INT" <>
        help    "Total coins in genesis"
    pure KO{..}

optsInfo :: ParserInfo KeygenOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to generate keyfiles"

main :: IO ()
main = do
    KO {..} <- execParser optsInfo
    let keysDir = takeDirectory koPattern
        genFileDir = takeDirectory koGenesisFile
    createDirectoryIfMissing True keysDir
    createDirectoryIfMissing True genFileDir

    genesisList <- forM [1..koStakeholders] $ \i ->
        generateKeyfile $ replace "{}" (show i) koPattern
    print $ show koStakeholders ++ " keyfiles are generated"

    let distr = TestnetStakes
            { sdTotalStake = mkCoin koTotalStake
            , sdRichmen    = koRichmen
            , sdPoor       = koStakeholders - koRichmen
            }
        genesisAddrs = map (makePubKeyAddress . fst) genesisList
        genesisVssCerts = HM.fromList
                          $ map (_1 %~ addressHash)
                          $ take (fromIntegral koRichmen) genesisList
        genData = GenesisData
            { gdAddresses = genesisAddrs
            , gdDistribution = distr
            , gdVssCertificates = genesisVssCerts
            }
    BSL.writeFile koGenesisFile $ encode genData
    case decodeFull (encode genData) of
        Right (_ :: GenesisData) ->
            putText "genesis.bin generated successfully\n"
        Left err                 -> do
            putText ("Generated genesis.bin can't be read: " <>
                     toText err <> "\n")
            if length (encode genData) < 10*1024
                then putText "Printing GenesisData:\n\n" >> print genData
                else putText "genesis.bin is bigger than 10k, won't print it\n"

