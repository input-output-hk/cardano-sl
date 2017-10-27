-- | Types defining a command in Auxx.

module Command.Types
       ( Command (..)
       , ProposeUpdateSystem (..)
       , SendMode (..)
       , SendToAllGenesisParams (..)
       , ProposeUpdateParams (..)
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
    { puIdx             :: Int -- TODO: what is this? rename
    , puBlockVersion    :: BlockVersion
    , puScriptVersion   :: ScriptVersion
    , puSlotDurationSec :: Int
    , puMaxBlockSize    :: Byte
    , puSoftwareVersion :: SoftwareVersion
    , puUpdates         :: [ProposeUpdateSystem]
    } deriving (Show)

data Command
    = Balance Address
    | Send Int (NonEmpty TxOut)
    | SendToAllGenesis !SendToAllGenesisParams
    | Vote Int Bool UpId
    | ProposeUpdate !ProposeUpdateParams
    | HashInstaller !FilePath
    | Help
    | ListAddresses
    | DelegateLight !Int !PublicKey !EpochIndex !(Maybe EpochIndex) !Bool
     -- ^ From whom, to whom, ttl start/end epochs, dry mode (off =
     -- send cert, on = dump yaml)
    | DelegateHeavy !Int !PublicKey !EpochIndex !Bool
     -- ^ From whom, to whom, ttl epoch, last argument is current
     -- epoch, dry mode
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | AddrDistr !PublicKey !AddrStakeDistribution
    | Rollback !Word !FilePath
    | SendTxsFromFile !FilePath
    | Quit
    deriving Show

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving Show
