-- | Types defining a command in Auxx.

module Command.Types
       ( Command (..)
       , ProposeUpdateSystem (..)
       , SendMode (..)
       , SendToAllGenesisParams (..)
       , GenBlocksParams (..)
       , ProposeUpdateParams (..)
       , PrintAction
       ) where

import           Universum

import           Pos.Crypto (PublicKey)
import           Pos.Txp    (TxOut)
import           Pos.Types  (AddrStakeDistribution, Address, BlockVersion, EpochIndex,
                             SoftwareVersion)
import           Pos.Update (BlockVersionModifier, SystemTag, UpId)

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
    { puSecretKeyIdx         :: !Int -- the node that creates/signs the proposal
    , puBlockVersion         :: !BlockVersion
    , puSoftwareVersion      :: !SoftwareVersion
    , puBlockVersionModifier :: !BlockVersionModifier
    , puUpdates              :: ![ProposeUpdateSystem]
    } deriving (Show)

data GenBlocksParams = GenBlocksParams
    { bgoBlockN :: !Word32
    -- ^ Number of blocks to generate.
    , bgoSeed   :: !(Maybe Int)
    -- ^ Generating seed.
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
    | DelegateLight !Int !PublicKey !EpochIndex !(Maybe EpochIndex)
     -- ^ From whom, to whom, ttl start/end epochs
    | DelegateHeavy !Int !PublicKey !EpochIndex !Bool
     -- ^ From whom, to whom, ttl epoch, last argument is current
     -- epoch, dry mode
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | AddrDistr !PublicKey !AddrStakeDistribution
    | Rollback !Word !FilePath
    | GenBlocks !GenBlocksParams
    | SendTxsFromFile !FilePath
    | PrintBlockVersionData
    | Quit
    deriving Show

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving Show

-- | An action used to print messages to the terminal. We can't hardcode
-- 'putText' because Haskeline defines its own printing method.
type PrintAction m = Text -> m ()
