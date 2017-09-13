-- | Types defining a command in Auxx.

module Command.Types
       ( Command (..)
       , ProposeUpdateSystem (..)
       , SendMode (..)
       , SendToAllGenesisParams (..)
       , ProposeUpdateParams (..)
       , ProposeUnlockStakeEpochParams (..)
       ) where

import           Universum

import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Types             (ScriptVersion)
import           Pos.Crypto                 (PublicKey)
import           Pos.Txp                    (TxOut)
import           Pos.Types                  (AddrStakeDistribution, Address, BlockVersion,
                                             EpochIndex, SoftwareVersion)
import           Pos.Update                 (SystemTag, UpId)

-- | Specify how transactions are sent to the network during
-- benchmarks using 'SendToAllGenesis'.
data SendMode =
      SendNeighbours -- ^ Send each transaction to every specified neighbour
    | SendRoundRobin -- ^ Send transactions to neighbours in a round-robin fashion
    | SendRandom     -- ^ Send each transaction to a randomly picked neighbour
    deriving Show

-- | Parameters for 'SendToAllGenesis' command.
data SendToAllGenesisParams = SendToAllGenesisParams
    { stagpDuration    :: !Int
    , stagpConc        :: !Int
    , stagpDelay       :: !Int
    , stagpMode        :: !SendMode
    , stagpTpsSentFile :: !FilePath
    } deriving (Show)

-- | Parameters for 'ProposeUpdate' command.
data ProposeUpdateParams = ProposeUpdateParams
    { puSecretKeyIdx    :: Int -- the node that creates/signs the proposal
    , puBlockVersion    :: BlockVersion
    , puScriptVersion   :: ScriptVersion
    , puSlotDurationSec :: Int
    , puMaxBlockSize    :: Byte
    , puSoftwareVersion :: SoftwareVersion
    , puUpdates         :: [ProposeUpdateSystem]
    } deriving (Show)

data ProposeUnlockStakeEpochParams = ProposeUnlockStakeEpochParams
    { puseSecretKeyIdx     :: Int -- the node that creates/signs the proposal
    , puseBlockVersion     :: BlockVersion
    , puseSoftwareVersion  :: SoftwareVersion
    , puseUnlockStakeEpoch :: EpochIndex
    } deriving (Show)

data Command
    = Balance Address
    | Send Int (NonEmpty TxOut)
    | SendToAllGenesis !SendToAllGenesisParams
    | Vote Int Bool UpId
    | ProposeUpdate !ProposeUpdateParams
    | ProposeUnlockStakeEpoch !ProposeUnlockStakeEpochParams
    | Help
    | ListAddresses
    | DelegateLight !Int !PublicKey !EpochIndex !(Maybe EpochIndex) -- first and last epoch of psk ttl
    | DelegateHeavy !Int !PublicKey !EpochIndex -- last argument is current epoch
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | AddrDistr !PublicKey !AddrStakeDistribution
    | Rollback !Word !FilePath
    | SendTxsFromFile !FilePath
    | PrintBlockVersionData
    | Quit
    deriving Show

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving Show
