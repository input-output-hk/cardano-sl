{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

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
import           Pos.Crypto           (RedeemPublicKey (..), keyGen, toPublic)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..))
import           Pos.Ssc.GodTossing   (vcSigningKey)
import           Pos.Txp.Core         (TxOutDistribution)
import           Pos.Types            (Address, Coin, StakeholderId, addressHash,
                                       makeRedeemAddress, unsafeAddCoin,
                                       unsafeIntegerToCoin)
import           Pos.Util             (runGen)
import           Pos.Util.UserSecret  (readUserSecret, usPrimKey)

getHolderId :: String -> [String] -> IO (Maybe StakeholderId)
getHolderId holder args = case holder of
    "fileholder" -> do
        fileName <- maybe
            (fail "No secret key filename is provided")
            pure $ head args
        sk <- maybe
            (fail "No secret key is found in file")
            pure =<< view usPrimKey <$> readUserSecret fileName
        pure $ Just . addressHash $ toPublic sk
    "randholder" -> do
        putText "USING RANDOM STAKEHOLDER ID."
        putText "NOT FOR PRODUCTION USAGE, ONLY FOR TESTING"
        putText "IF YOU INTEND TO GENERATE GENESIS.BIN FOR PRODUCTION, \
                \STOP RIGHT HERE AND USE `fileholder <path to secret>` OPTION \
                \INSTEAD OF `randholder`. THIS IS SERIOUS."
        Just . addressHash . fst <$> keyGen
    "noholder" -> pure Nothing
    _ -> error $ "unknown 'holder' parameter" <> toText holder

main :: IO ()
main = do
    args <- getArgs
    -- TODO: use optparse-applicative (or not if it's a throwaway tool)
    case args of
        (fpath:outpath:mbcerts:holder:restArgs) -> do
            jsonfile <- BSL.readFile fpath
            case A.eitherDecode jsonfile of
                Left err       -> error (toText err)
                Right avvmData -> do
                    holderId <- getHolderId holder restArgs
                    let genesis = genGenesis avvmData (mbcerts == "randcerts") holderId
                    createDirectoryIfMissing True (takeDirectory outpath)
                    BSL.writeFile outpath (Bi.encode genesis)
        _ -> do
            putStrLn $ unlines [
                "cardano-avvmmigrate",
                "",
                "Usage: ",
                "  cardano-avvmmigrate <path to JSON> <.bin output file> \
                \(nocerts|randcerts) (noholder|randholder|fileholder <path to secret file>)"
                ]
            exitFailure

data AvvmData = AvvmData
    { utxo :: [AvvmEntry]
    } deriving (Show, Generic)

instance FromJSON AvvmData

data AvvmCoin = AvvmCoin
    { coinAmount :: Integer
    , coinColor  :: Integer
    } deriving (Show, Generic)

instance FromJSON AvvmCoin where
    parseJSON = withObject "coin" $ \o -> do
        coinAmount <- o .: "coinAmount"
        coinColor <- o .: "coinColor" >>= (.: "getColor")
        return AvvmCoin{..}

data AvvmEntry = AvvmEntry
    { coin    :: AvvmCoin
    , address :: Text
    } deriving (Show, Generic)

instance FromJSON AvvmEntry

genGenesis
    :: AvvmData
    -> Bool                 -- ^ Whether to generate random certificates
    -> Maybe StakeholderId  -- ^ A stakeholder to which to delegate the distribution
    -> GenesisData
genGenesis avvm genCerts holder = GenesisData
    { gdAddresses = HM.keys balances
    , gdDistribution = ExplicitStakes balances
    , gdVssCertificates = if genCerts then randCerts else mempty
    }
  where
    distr = maybe (const []) (\id -> pure . (id, )) holder
    randCerts = HM.fromList [(addressHash (vcSigningKey c), c)
                            | c <- runGen (replicateM 10 arbitrary)]

    sumDistrs :: TxOutDistribution -> TxOutDistribution -> TxOutDistribution
    sumDistrs (HM.fromList -> h1) (HM.fromList -> h2) =
        HM.toList $ HM.unionWith unsafeAddCoin h1 h2

    sumOutcomes
        :: (Coin, TxOutDistribution)
        -> (Coin, TxOutDistribution)
        -> (Coin, TxOutDistribution)
    sumOutcomes = uncurry bimap . bimap unsafeAddCoin sumDistrs

    balances :: HashMap Address (Coin, TxOutDistribution)
    balances = HM.fromListWith sumOutcomes $ do
        AvvmEntry{..} <- utxo avvm
        let pk = case decodeUrl address of
                Right x -> RedeemPublicKey (Ed.PublicKey x)
                Left _  -> error ("couldn't decode address " <> address)
        let addr = makeRedeemAddress pk
            adaCoin = unsafeIntegerToCoin $ coinAmount coin
        return (addr, (adaCoin, distr adaCoin))
