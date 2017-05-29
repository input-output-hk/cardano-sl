{-# LANGUAGE ScopedTypeVariables #-}

-- | Former avvm-migration script. Parses rscoin-dump to create
-- genesis block of CSL.

module Avvm
       ( AvvmData (..)
       , AvvmEntry (..)
       , genGenesis
       , applyBlacklisted
       , getHolderId
       ) where

import           Data.Aeson           (FromJSON (..), withObject, (.:))
import qualified Data.ByteString      as BS
import qualified Data.HashMap.Strict  as HM
import           Data.List            ((\\))
import qualified Data.Text            as T
import qualified Serokell.Util.Base64 as B64
import           System.Random        (randomRIO)
import           Test.QuickCheck      (arbitrary)
import           Universum

import qualified Pos.Binary.Class     as Bi
import           Pos.Crypto           (RedeemPublicKey (..), keyGen, redeemPkBuild,
                                       toPublic)
import           Pos.Genesis          (GenesisData (..), GenesisGtData (..),
                                       StakeDistribution (..))
import           Pos.Ssc.GodTossing   (vcSigningKey)
import           Pos.Txp.Core         (TxOutDistribution)
import           Pos.Types            (Address, Coin, StakeholderId, addressHash,
                                       makeRedeemAddress, unsafeAddCoin,
                                       unsafeIntegerToCoin)
import           Pos.Util             (runGen)
import           Pos.Util.UserSecret  (readUserSecret, usPrimKey)
import           System.Wlog          (WithLogger)


-- | Read the text into a redeeming public key.
--
-- There's also a copy of this function in cardano-addr-convert.
fromAvvmPk :: (MonadFail m, Monad m) => Text -> m RedeemPublicKey
fromAvvmPk addrText = do
    let base64rify = T.replace "-" "+" . T.replace "_" "/"
    let parsedM = B64.decode $ base64rify addrText
    addrParsed <-
        maybe (fail $ "Address " <> toString addrText <> " is not base64(url) format")
        pure
        (rightToMaybe parsedM)
    unless (BS.length addrParsed == 32) $
        fail "Address' length is not equal to 32, can't be redeeming pk"
    pure $ redeemPkBuild addrParsed

data AvvmData = AvvmData
    { utxo :: [AvvmEntry]
    } deriving (Show, Generic)

instance FromJSON AvvmData

{- I will use rscoin's dump-state(-new) format for now which doesn't use colored coins,
data AvvmCoin = AvvmCoin
    { coinAmount :: Integer
    , coinColor  :: Integer
    } deriving (Show, Generic)

instance FromJSON AvvmCoin where
    parseJSON = withObject "coin" $ \o -> do
        coinAmount <- o .: "coinAmount"
        coinColor <- o .: "coinColor" >>= (.: "getColor")
        return AvvmCoin{..}
-}

data AvvmEntry = AvvmEntry
    { aeCoin      :: !Integer         -- in lovelaces
    , aePublicKey :: !RedeemPublicKey -- in base64(u), yep
    } deriving (Show, Generic, Eq)

instance FromJSON AvvmEntry where
    parseJSON = withObject "avvmEntry" $ \o -> do
        aeCoin <- (* (1000000 :: Integer)) <$> o .: "coin"
        (addrText :: Text) <- o .: "address"
        aePublicKey <- fromAvvmPk addrText
        return AvvmEntry{..}

-- | Generate genesis data out of avvm parameters.
genGenesis
    :: AvvmData
    -> Bool           -- ^ Whether to generate random certificates
    -> StakeholderId  -- ^ A stakeholder to which to delegate the distribution
    -> (GenesisData, GenesisGtData)
genGenesis avvm genCerts holder =
    ( GenesisData
        { gdAddresses = HM.keys balances
        , gdDistribution = ExplicitStakes balances
        , gdBootstrapBalances = mempty
        }
    , GenesisGtData
        { ggdVssCertificates = if genCerts then randCerts else mempty
        }
    )
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
    sumOutcomes (c1, t1) (c2, t2) = (unsafeAddCoin c1 c2, sumDistrs t1 t2)

    balances :: HashMap Address (Coin, TxOutDistribution)
    balances = HM.fromListWith sumOutcomes $ do
        AvvmEntry{..} <- utxo avvm
        let addr = makeRedeemAddress aePublicKey
            adaCoin = unsafeIntegerToCoin aeCoin
        return (addr, (adaCoin, distr adaCoin))

-- | Applies blacklist to avvm utxo, produces warnings and stats about
-- how much was deleted.
applyBlacklisted :: Maybe FilePath -> AvvmData -> IO AvvmData
applyBlacklisted Nothing r = r <$ putText "Blacklisting: file not specified, skipping"
applyBlacklisted (Just blacklistPath) AvvmData{..} = do
    addrTexts <- lines <$> readFile blacklistPath
    blacklisted <- mapM fromAvvmPk addrTexts
    let filteredBad = filter ((`elem` blacklisted) . aePublicKey) utxo
    let filtered = utxo \\ filteredBad
    putText $
        "Removing " <> show (length filteredBad) <> " entries from utxo (out of " <>
        show (length blacklisted) <> " total entries in the blacklist)"
    pure $ AvvmData filtered

getHolderId :: (MonadIO m, WithLogger m) => Maybe FilePath -> m StakeholderId
getHolderId (Just fileName) = do
    mSk <- view usPrimKey <$> readUserSecret fileName
    let sk = fromMaybe (error "No secret key is found in file") mSk
    pure $ addressHash $ toPublic sk
getHolderId Nothing = do
    skPath <- liftIO $ ("redeemingHolderKey" <>) . show <$> randomRIO (0,100000::Int)
    (pk,sk) <- keyGen
    putText $ "USING RANDOM STAKEHOLDER ID, WRITING KEY TO " <> fromString skPath
    putText "NOT FOR PRODUCTION USAGE, ONLY FOR TESTING"
    putText "IF YOU INTEND TO GENERATE GENESIS FOR PRODUCTION, \
            \STOP RIGHT HERE AND USE `--fileholder <path to secret>` OPTION. \
            \THIS IS SERIOUS."
    liftIO $ BS.writeFile skPath $ Bi.encodeStrict sk
    pure $ addressHash pk
