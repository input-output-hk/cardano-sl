-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( BalanceDistribution (..)

       , GenesisWStakeholders (..)
       , GenesisDelegation (..)
       , GenesisVssCertificatesMap (..)
       , noGenesisDelegation
       , mkGenesisDelegation
       , bootDustThreshold

         -- * GenesisSpec
       , TestnetBalanceOptions (..)
       , FakeAvvmOptions (..)
       , TestnetDistribution (..)
       , GenesisInitializer (..)
       , GenesisAvvmBalances (..)
       , GenesisNonAvvmBalances (..)
       , ProtocolConstants (..)
       , GenesisSpec (..)
       , convertNonAvvmDataToBalances
       , mkGenesisSpec

       -- * GenesisData
       , GenesisData (..)
       , genesisDataFromSpec
       ) where

import           Universum

import           Control.Lens             (at)
import           Control.Monad.Except     (MonadError (throwError))
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, build, sformat, (%))
import           Serokell.Util            (allDistinct, mapJson)

import           Pos.Binary.Class         (Bi)
import           Pos.Core.Address         (addressHash, decodeTextAddress)
import           Pos.Core.Coin            (unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Types           (Address, BlockVersionData, Coin, ProxySKHeavy,
                                           SharedSeed, StakeholderId, Timestamp)
import           Pos.Core.Vss.Types       (VssCertificatesMap)
import           Pos.Crypto.Signing.Types (ProxySecretKey (..), RedeemPublicKey,
                                           isSelfSignedPsk)

-- | Balances distribution in genesis block.
data BalanceDistribution =
    -- | Rich/poor distribution, for testnet mostly.
    RichPoorBalances
        { sdRichmen     :: !Word
        , sdRichBalance :: !Coin
        , sdPoor        :: !Word
        , sdPoorBalance :: !Coin
        }
    deriving (Show, Eq, Generic)

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: Map StakeholderId Word16
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%mapJson) m

-- | This type contains genesis state of heavyweight delegation. It
-- wraps a map where keys are issuers (i. e. stakeholders who
-- delegated) and values are proxy signing keys. There are some invariants:
-- 1. In each pair delegate must differ from issuer, i. e. no revocations.
-- 2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--    equal to the key in the map.
-- 3. Delegates can't be issuers, i. e. transitive delegation is not supported.
--    It's not needed in genesis, it can always be reduced.
newtype GenesisDelegation = UnsafeGenesisDelegation
    { unGenesisDelegation :: HashMap StakeholderId ProxySKHeavy
    } deriving (Show, Eq, Monoid)

-- | Empty 'GenesisDelegation'.
noGenesisDelegation :: GenesisDelegation
noGenesisDelegation = UnsafeGenesisDelegation mempty

-- | Safe constructor of 'GenesisDelegation'.
mkGenesisDelegation ::
       MonadError Text m
    => HashMap StakeholderId ProxySKHeavy
    -> m GenesisDelegation
mkGenesisDelegation pskM = do
    forM_ (HM.toList pskM) $ \(k, ProxySecretKey{..}) ->
        when (addressHash pskIssuerPk /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (addressHash pskIssuerPk)
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    when (any isSelfSignedPsk psks) $
        throwError "there is a self-signed (revocation) psk"
    let resPairs =
            psks <&> \psk@ProxySecretKey {..} -> (addressHash pskIssuerPk, psk)
    let resMap = HM.fromList resPairs
    let isIssuer ProxySecretKey {..} =
            isJust $ resMap ^. at (addressHash pskDelegatePk)
    when (any isIssuer psks) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation resMap
  where
    psks = toList pskM

-- | Calculates a minimum amount of coins user can set as an output in
-- boot era.
bootDustThreshold :: GenesisWStakeholders -> Coin
bootDustThreshold (GenesisWStakeholders bootHolders) =
    -- it's safe to use it here because weights are word16 and should
    -- be really low in production, so this sum is not going to be
    -- even more than 10-15 coins.
    unsafeIntegerToCoin . sum $ map fromIntegral $ toList bootHolders

----------------------------------------------------------------------------
-- Genesis Spec
----------------------------------------------------------------------------

-- | These options determine balances of nodes specific for testnet.
data TestnetBalanceOptions = TestnetBalanceOptions
    { tboPoors          :: !Word
    -- ^ Number of poor nodes (with small balance).
    , tboRichmen        :: !Word
    -- ^ Number of rich nodes (with huge balance).
    , tboTotalBalance   :: !Word64
    -- ^ Total balance owned by these nodes.
    , tboRichmenShare   :: !Double
    -- ^ Portion of stake owned by all richmen together.
    , tboUseHDAddresses :: !Bool
    -- ^ Whether generate plain addresses or with hd payload.
    } deriving (Show)

-- | These options determines balances of fake AVVM nodes which didn't
-- really go through vending, but pretend they did.
data FakeAvvmOptions = FakeAvvmOptions
    { faoCount      :: !Word
    , faoOneBalance :: !Word64
    } deriving (Show)

-- | This data type determines how to generate bootstrap stakeholders
-- in testnet.
data TestnetDistribution
    = TestnetRichmenStakeDistr
    -- ^ Rich nodes will be bootstrap stakeholders with equal weights.
    | TestnetCustomStakeDistr
    { tcsdBootStakeholders :: !GenesisWStakeholders
    -- ^ Bootstrap stakeholders and their weights are provided explicitly.
    , tcsdVssCerts         :: !GenesisVssCertificatesMap
    -- ^ Vss certificates are provided explicitly too, because they
    -- can't be generated automatically in this case.
    } deriving (Show)

-- | This data type contains various options presense of which depends
-- on whether we want genesis for mainnet or testnet.
data GenesisInitializer
    = TestnetInitializer {
      tiTestBalance     :: !TestnetBalanceOptions
    , tiFakeAvvmBalance :: !FakeAvvmOptions
    , tiDistribution    :: !TestnetDistribution
    , tiSeed            :: !Integer
      -- ^ Seed to use to generate secret data. It's used only in
      -- testnet, shouldn't be used for anything important.
    }
    | MainnetInitializer {
      miStartTime        :: !Timestamp
    , miBootStakeholders :: !GenesisWStakeholders
    , miVssCerts         :: !GenesisVssCertificatesMap
    , miNonAvvmBalances  :: !GenesisNonAvvmBalances
    } deriving (Show)

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Monoid)

-- | Predefined balances of non avvm entries.
newtype GenesisVssCertificatesMap = GenesisVssCertificatesMap
    { getGenesisVssCertificatesMap :: VssCertificatesMap
    } deriving (Show, Eq, Monoid)

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: HashMap Address Coin
    } deriving (Show, Eq)

deriving instance Bi Address => Monoid GenesisNonAvvmBalances

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadFail m, Bi Address )
    => HashMap Text Integer
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> balances'
  where
    balances' :: m (HashMap Address Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert (HM.toList balances)
    convert :: (Text, Integer) -> m (Address, Coin)
    convert (txt, i) = case decodeTextAddress txt of
        Left err   -> fail (toString err)
        Right addr -> return (addr, unsafeIntegerToCoin i)

-- | 'ProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data ProtocolConstants = ProtocolConstants
    { -- | Security parameter from the paper.
      pcK             :: !Int
      -- | Magic constant for separating real/testnet.
    , pcProtocolMagic :: !Int32
      -- | VSS certificates max timeout to live (number of epochs).
    , pcVssMaxTTL     :: !Word32
      -- | VSS certificates min timeout to live (number of epochs).
    , pcVssMinTTL     :: !Word32
    } deriving (Show, Eq, Generic)

-- | Specification how to generate full genesis data.
data GenesisSpec = UnsafeGenesisSpec
    { gsAvvmDistr         :: !GenesisAvvmBalances
    -- ^ Genesis data describes avvm utxo.
    , gsFtsSeed           :: !SharedSeed
    -- ^ Seed for FTS for 0-th epoch.
    , gsHeavyDelegation   :: !GenesisDelegation
    -- ^ Genesis state of heavyweight delegation.
    , gsBlockVersionData  :: !BlockVersionData
    -- ^ Genesis 'BlockVersionData'.
    , gsProtocolConstants :: !ProtocolConstants
    -- ^ Other constants which affect consensus.
    , gsInitializer       :: !GenesisInitializer
    -- ^ Other data which depend on genesis type.
    } deriving (Show, Generic)

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec
    :: GenesisAvvmBalances
    -> SharedSeed
    -> GenesisDelegation
    -> BlockVersionData
    -> ProtocolConstants
    -> GenesisInitializer
    -> Either String GenesisSpec
mkGenesisSpec avvmDistr seed delega bvd pc specType = do
    let avvmKeys = HM.keys $ getGenesisAvvmBalances avvmDistr
    unless (allDistinct avvmKeys) $
        throwError $ "mkGenesisSpec: there are duplicates in avvm balances"

    -- All checks passed
    pure $ UnsafeGenesisSpec avvmDistr seed delega bvd pc specType

----------------------------------------------------------------------------
-- GenesisData
----------------------------------------------------------------------------

-- | Genesis data contains all data which determines consensus
-- rules. It must be same for all nodes. It's used to initialize
-- global state, slotting, etc.
data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !Timestamp
    , gdVssCerts         :: !GenesisVssCertificatesMap
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !ProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    , gdFtsSeed          :: !SharedSeed
    } deriving (Show, Eq)

-- | 'GenesisData' is determined by 'GenesisSpec' whenever it has a
-- 'MainnetInitializer'.
genesisDataFromSpec :: GenesisSpec -> Maybe GenesisData
genesisDataFromSpec UnsafeGenesisSpec{..}
  | MainnetInitializer{..} <- gsInitializer =
        let gdBootStakeholders = miBootStakeholders
            gdHeavyDelegation = gsHeavyDelegation
            gdStartTime = miStartTime
            gdVssCerts = miVssCerts
            gdNonAvvmBalances = miNonAvvmBalances
            gdBlockVersionData = gsBlockVersionData
            gdProtocolConsts = gsProtocolConstants
            gdAvvmDistr = gsAvvmDistr
            gdFtsSeed = gsFtsSeed
        in  Just GenesisData{..}
  | otherwise = Nothing
