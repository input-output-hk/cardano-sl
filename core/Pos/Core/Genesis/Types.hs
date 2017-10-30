-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       (
         GenesisWStakeholders (..)
       , GenesisDelegation (..)
       , GenesisVssCertificatesMap (..)
       , noGenesisDelegation

         -- * GenesisSpec
       , TestnetBalanceOptions (..)
       , FakeAvvmOptions (..)
       , GenesisInitializer (..)
       , GenesisAvvmBalances (..)
       , GenesisNonAvvmBalances (..)
       , ProtocolConstants (..)
       , GenesisSpec (..)
       , mkGenesisSpec

       -- * GenesisData
       , GenesisData (..)
       , genesisDataFromSpec
       ) where

import           Universum

import           Control.Monad.Except     (MonadError (throwError))
import           Data.Hashable            (Hashable)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text.Buildable      as Buildable
import           Fmt                      (genericF)
import           Formatting               (bprint, build, fixed, int, (%))
import           Serokell.Util            (allDistinct, mapJson)

import           Pos.Binary.Crypto        ()
import           Pos.Core.Coin            ()
import           Pos.Core.Types           (Address, BlockVersionData, Coin, CoinPortion,
                                           ProxySKHeavy, SharedSeed, StakeholderId,
                                           Timestamp)
import           Pos.Core.Vss.Types       (VssCertificatesMap, getVssCertificatesMap)
import           Pos.Crypto.Signing.Types (RedeemPublicKey)

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
--
-- Each 'Word16' is a weight. I.e. if stakeholder A has weight "1"
-- and stakeholder B has weight "3", during the bootstrap era
-- all stake in the system will be divided between A and B
-- in proportion of 1:3.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: Map StakeholderId Word16
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%mapJson) m

-- | Predefined balances of non avvm entries.
newtype GenesisVssCertificatesMap = GenesisVssCertificatesMap
    { getGenesisVssCertificatesMap :: VssCertificatesMap
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisVssCertificatesMap where
    build (GenesisVssCertificatesMap m) =
        bprint ("GenesisVssCertificatesMap: "%mapJson) (getVssCertificatesMap m)

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
    } deriving (Show, Eq)

-- | Empty 'GenesisDelegation'.
noGenesisDelegation :: GenesisDelegation
noGenesisDelegation = UnsafeGenesisDelegation mempty

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

instance Buildable TestnetBalanceOptions where
    build TestnetBalanceOptions {..} =
        bprint
            (int%" poor guys, "%
             int%" rich guys , "%
             "total balance is "%int%
             ", richmen share is "%fixed 3%
             " and useHDAddresses flag is " %build)

            tboPoors
            tboRichmen
            tboTotalBalance
            tboRichmenShare
            tboUseHDAddresses

-- | These options determines balances of fake AVVM nodes which didn't
-- really go through vending, but pretend they did.
data FakeAvvmOptions = FakeAvvmOptions
    { faoCount      :: !Word
    , faoOneBalance :: !Word64
    } deriving (Show, Generic)

instance Buildable FakeAvvmOptions where
    build = genericF

-- | This data type contains various options presense of which depends
-- on whether we want genesis for mainnet or testnet.
data GenesisInitializer
    = TestnetInitializer {
      tiTestBalance       :: !TestnetBalanceOptions
    , tiFakeAvvmBalance   :: !FakeAvvmOptions
    , tiAvvmBalanceFactor :: !CoinPortion
    -- ^ Avvm balances will be multiplied by this factor.
    , tiSeed              :: !Integer
      -- ^ Seed to use to generate secret data. It's used only in
      -- testnet, shouldn't be used for anything important.
    }
    | MainnetInitializer {
      miStartTime        :: !Timestamp
    , miBootStakeholders :: !GenesisWStakeholders
    , miVssCerts         :: !GenesisVssCertificatesMap
    , miNonAvvmBalances  :: !GenesisNonAvvmBalances
    } deriving (Show)

instance (Hashable Address, Buildable Address) =>
         Buildable GenesisInitializer where
    build =
        \case
            TestnetInitializer {..} ->
                bprint
                    ("TestnetInitializer {\n"%
                     "  "%build%"\n"%
                     "  "%build%"\n"%
                     "  avvm balance factor: "%build%"\n"%
                     "  seed: "%int%"\n"%
                     "}\n"
                    )

                    tiTestBalance
                    tiFakeAvvmBalance
                    tiAvvmBalanceFactor
                    tiSeed
            MainnetInitializer {..} ->
                bprint
                    ("MainnetInitializer {\n"%
                     "  system start: "%build%"\n" %
                     "  bootstrap stakeholders: "%build%"\n"%
                     "  vss certificates: "%build%"\n"%
                     "  non-avvm balances: "%build%"\n"%
                     "}\n"
                    )
                    miStartTime
                    miBootStakeholders
                    miVssCerts
                    miNonAvvmBalances

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Monoid)

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: HashMap Address Coin
    } deriving (Show, Eq)

instance (Hashable Address, Buildable Address) =>
         Buildable GenesisNonAvvmBalances where
    build (GenesisNonAvvmBalances m) =
        bprint ("GenesisNonAvvmBalances: " %mapJson) m

deriving instance Hashable Address => Monoid GenesisNonAvvmBalances

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
