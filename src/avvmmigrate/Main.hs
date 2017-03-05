{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Crypto.Sign.Ed25519  as Ed
import           Data.Aeson           (FromJSON (..), withObject, (.:))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Serokell.Util.Base64 (decodeUrl)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)
import           Test.QuickCheck      (arbitrary)
import           Universum

import qualified Pos.Binary           as Bi
import           Pos.Crypto           (PublicKey (..))
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..))
import           Pos.Ssc.GodTossing   (vcSigningKey)
import           Pos.Types            (Address, Coin, addressHash, makePubKeyAddress,
                                       unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util             (runGen)

main :: IO ()
main = do
    args <- getArgs
    -- TODO: use optparse-applicative (or not if it's a throwaway tool)
    case args of
        [fpath, outpath, mbcerts] -> do
            jsonfile <- BSL.readFile fpath
            case A.eitherDecode jsonfile of
                Left err       -> panic (toText err)
                Right avvmData -> do
                    let genesis = genGenesis avvmData (mbcerts == "randcerts")
                    createDirectoryIfMissing True (takeDirectory outpath)
                    BSL.writeFile outpath (Bi.encode genesis)
        _ -> do
            putStrLn $ unlines [
                "cardano-avvmmigrate",
                "",
                "Usage: ",
                "  cardano-avvmmigrate <path to JSON> <.bin output file> (nocerts|randcerts)"
                ]
            exitFailure

data AvvmData = AvvmData {
    utxo :: [AvvmEntry] }
    deriving (Show, Generic)

instance FromJSON AvvmData

data AvvmCoin = AvvmCoin {
    coinAmount :: Integer,
    coinColor  :: Integer }
    deriving (Show, Generic)

instance FromJSON AvvmCoin where
    parseJSON = withObject "coin" $ \o -> do
        coinAmount <- o .: "coinAmount"
        coinColor <- o .: "coinColor" >>= (.: "getColor")
        return AvvmCoin{..}

data AvvmEntry = AvvmEntry {
    coin    :: AvvmCoin,
    address :: Text }
    deriving (Show, Generic)

instance FromJSON AvvmEntry

genGenesis
    :: AvvmData
    -> Bool              -- ^ Whether to generate random certificates
    -> GenesisData
genGenesis avvm genCerts = GenesisData
    { gdAddresses = HM.keys balances
    , gdDistribution = ExplicitStakes balances
    , gdVssCertificates = if genCerts then randCerts else mempty
    }
  where
    randCerts = HM.fromList [(addressHash (vcSigningKey c), c)
                            | c <- runGen (replicateM 10 arbitrary)]
    balances :: HashMap Address Coin
    balances = HM.fromListWith unsafeAddCoin $ do
        AvvmEntry{..} <- utxo avvm
        let pk = case decodeUrl address of
                Right x -> PublicKey (Ed.PublicKey x)
                Left _  -> panic ("couldn't decode address " <> address)
        let addr = makePubKeyAddress pk
        return (addr, unsafeIntegerToCoin (coinAmount coin))
