module Avvm
       ( AvvmData (..)
       , AvvmCoin (..)
       , AvvmEntry (..)
       , genGenesis
       , getHolderId
       ) where

import qualified Crypto.Sign.Ed25519  as Ed
import           Data.Aeson           (FromJSON (..), withObject, (.:))
import qualified Data.HashMap.Strict  as HM
import           Serokell.Util.Base64 (decodeUrl)
import           Test.QuickCheck      (arbitrary)
import           Universum

import           Pos.Crypto           (RedeemPublicKey (..), keyGen, toPublic)
import           Pos.Genesis          (GenesisData (..), StakeDistribution (..))
import           Pos.Ssc.GodTossing   (vcSigningKey)
import           Pos.Txp.Core         (TxOutDistribution)
import           Pos.Types            (Address, Coin, StakeholderId, addressHash,
                                       makeRedeemAddress, unsafeAddCoin,
                                       unsafeIntegerToCoin)
import           Pos.Util             (runGen)
import           Pos.Util.UserSecret  (readUserSecret, usPrimKey)

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
    -> Bool           -- ^ Whether to generate random certificates
    -> StakeholderId  -- ^ A stakeholder to which to delegate the distribution
    -> GenesisData
genGenesis avvm genCerts holder = GenesisData
    { gdAddresses = HM.keys balances
    , gdDistribution = ExplicitStakes balances
    , gdVssCertificates = if genCerts then randCerts else mempty
    }
  where
    distr = pure . (holder, )
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

getHolderId :: Maybe FilePath -> IO StakeholderId
getHolderId holderPath = case holderPath of
    Just fileName -> do
        mSk <- view usPrimKey <$> readUserSecret fileName
        sk <- maybe (fail "No secret key is found in file")
              pure mSk
        pure $ addressHash $ toPublic sk
    Nothing -> do
        putText "USING RANDOM STAKEHOLDER ID."
        putText "NOT FOR PRODUCTION USAGE, ONLY FOR TESTING"
        putText "IF YOU INTEND TO GENERATE GENESIS.BIN FOR PRODUCTION, \
                \STOP RIGHT HERE AND USE `--fileholder <path to secret>` OPTION. \
                \THIS IS SERIOUS."
        addressHash . fst <$> keyGen
