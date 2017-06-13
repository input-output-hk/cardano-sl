-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NetworkParams (..)
       , NodeParams (..)
       ) where

import           Universum

import qualified Network.Transport.TCP   as TCP
import           System.Wlog             (LoggerName)

import           Pos.Communication.Types (NodeId)
import           Pos.Crypto              (SecretKey)
import           Pos.DHT.Real            (KademliaParams)
import           Pos.Security.Params     (SecurityParams)
import           Pos.Txp.Toil.Types      (Utxo)
import           Pos.Types               (Timestamp)
import           Pos.Update.Params       (UpdateParams)
import           Pos.Util.UserSecret     (UserSecret)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName        -- ^ Prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)  -- ^ Prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)  -- ^ Path to logger configuration
    , lpEkgPort       :: !(Maybe Int)
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpLoggingParams   :: !LoggingParams  -- ^ Logger parameters
    } deriving (Show)

-- | Network parameters.
data NetworkParams = NetworkParams
    { npDiscovery :: !(Either (Set NodeId) KademliaParams)
    -- ^ If this value is 'Left', then given peers will be used in static mode.
    -- Otherwise kademlia with given params will be used.
    , npTcpAddr   :: !TCP.TCPAddr
    -- ^ External TCP address of the node.
    -- It encapsulates bind address and address visible to other nodes.
    }

-- | This data type contains all data necessary to launch node and
-- known in advance (from CLI, configs, etc.)
data NodeParams = NodeParams
    { npDbPathM        :: !FilePath          -- ^ Path to node's database.
    , npRebuildDb      :: !Bool              -- ^ @True@ if data-base should be rebuilt
    , npSystemStart    :: !Timestamp         -- ^ System start
    , npSecretKey      :: !SecretKey         -- ^ Primary secret key of node
    , npUserSecret     :: !UserSecret        -- ^ All node secret keys
    , npBaseParams     :: !BaseParams        -- ^ See 'BaseParams'
    , npCustomUtxo     :: !Utxo              -- ^ predefined custom utxo
    , npJLFile         :: !(Maybe FilePath)  -- @georgeee please write comment to this field when you see this sign, i made it very long on purpose so it won't fit even in your huge monitor
    , npPropagation    :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , npReportServers  :: ![Text]            -- ^ List of report server URLs
    , npUpdateParams   :: !UpdateParams      -- ^ Params for update system
    , npSecurityParams :: !SecurityParams    -- ^ Params for "Pos.Security"
    , npUseNTP         :: !Bool
    , npNetwork        :: !NetworkParams
    -- ^ Network parameters.
    } -- deriving (Show)
