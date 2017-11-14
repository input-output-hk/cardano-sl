module Lang.Value
       (
       -- * Run-time values
         Value(..)
       , _ValueUnit
       , _ValueNumber
       , _ValueString
       , _ValueBool
       , _ValueList
       , _ValueAddress
       , _ValuePublicKey
       , _ValueTxOut
       , _ValueStakeholderId
       , _ValueHash
       , _ValueBlockVersion
       , _ValueSoftwareVersion
       , _ValueBlockVersionData
       , _ValueBlockVersionModifier
       , _ValueProposeUpdateSystem
       , _ValueAddrDistrPart
       , _ValueAddrStakeDistribution
       , _ValueFilePath
       , _ValueSendMode

       -- * Command-specific types
       , SendMode(..)
       , AddrDistrPart(..)
       , ProposeUpdateParams(..)
       , RollbackParams(..)
       , ProposeUpdateSystem(..)
       , GenBlocksParams(..)
       , AddKeyParams (..)

       ) where

import           Universum

import           Control.Lens (makePrisms)
import           Data.Scientific (Scientific)

import           Pos.Core (AddrStakeDistribution, Address, BlockVersion, CoinPortion,
                           SoftwareVersion, StakeholderId)
import           Pos.Core.Txp (TxOut)
import           Pos.Crypto (AHash, PublicKey)
import           Pos.Update (BlockVersionData, BlockVersionModifier, SystemTag)

-- | Specify how transactions are sent to the network during
-- benchmarks using 'SendToAllGenesis'.
data SendMode =
      SendNeighbours -- ^ Send each transaction to every specified neighbour
    | SendRoundRobin -- ^ Send transactions to neighbours in a round-robin fashion
    | SendRandom     -- ^ Send each transaction to a randomly picked neighbour
    deriving (Eq, Ord, Show)

data AddrDistrPart = AddrDistrPart
    { adpStakeholderId :: !StakeholderId
    , adpCoinPortion   :: !CoinPortion
    } deriving (Eq, Ord, Show)

-- | Parameters for 'ProposeUpdate' command.
data ProposeUpdateParams = ProposeUpdateParams
    { puSecretKeyIdx         :: !Int -- the node that creates/signs the proposal
    , puVoteAll              :: !Bool
    , puBlockVersion         :: !BlockVersion
    , puSoftwareVersion      :: !SoftwareVersion
    , puBlockVersionModifier :: !BlockVersionModifier
    , puUpdates              :: ![ProposeUpdateSystem]
    } deriving (Show)

data RollbackParams = RollbackParams
    { rpNum      :: !Word
    , rpDumpPath :: !FilePath
    } deriving (Show)

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving (Eq, Ord, Show)

data GenBlocksParams = GenBlocksParams
    { bgoBlockN :: !Word32
    -- ^ Number of blocks to generate.
    , bgoSeed   :: !(Maybe Int)
    -- ^ Generating seed.
    } deriving (Show)

-- | Parameters of `add-key` command.
data AddKeyParams = AddKeyParams
    { akpFile    :: !FilePath
    -- ^ Path to 'UserSecret'.
    , akpPrimary :: !Bool
    -- ^ If 'True', then primary key will be added.
    }

data Value
    = ValueUnit
    | ValueNumber Scientific
    | ValueString String
    | ValueBool Bool
    | ValueList [Value]
    | ValueAddress Address
    | ValuePublicKey PublicKey
    | ValueTxOut TxOut
    | ValueStakeholderId StakeholderId
    | ValueHash AHash
    | ValueBlockVersion BlockVersion
    | ValueSoftwareVersion SoftwareVersion
    | ValueBlockVersionModifier BlockVersionModifier
    | ValueBlockVersionData BlockVersionData
    | ValueProposeUpdateSystem ProposeUpdateSystem
    | ValueAddrDistrPart AddrDistrPart
    | ValueAddrStakeDistribution AddrStakeDistribution
    | ValueFilePath FilePath
    | ValueSendMode SendMode
    deriving (Eq, Ord, Show)

makePrisms ''Value
