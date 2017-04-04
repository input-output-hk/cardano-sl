-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       ) where

import           Pos.Util.TimeWarp   (NetworkAddress)
import           System.Wlog         (LoggerName)
import           Universum

import           Pos.Crypto          (SecretKey)
import           Pos.DHT.Model.Types (DHTKey, DHTNode)
import           Pos.Security.CLI    (AttackTarget, AttackType)
import           Pos.Txp.Toil.Types  (Utxo)
import           Pos.Types           (Timestamp)
import           Pos.Update.Params   (UpdateParams)
import           Pos.Util.UserSecret (UserSecret)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName        -- ^ Prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)  -- ^ Prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)  -- ^ Path to logger configuration
    , lpEkgPort       :: !(Maybe Int)
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpIpPort             :: !(Maybe NetworkAddress) -- ^ Port to run on
    , bpDHTPeers           :: ![DHTNode]      -- ^ Peers passed from CLI
    , bpDHTKey             :: !(Maybe DHTKey)
    , bpDHTExplicitInitial :: !Bool
    , bpLoggingParams      :: !LoggingParams  -- ^ Logger parameters
    , bpKademliaDump       :: !FilePath       -- ^ Path to kademlia dump file
    } deriving (Show)

-- | Contains algorithm specific & storage parameters for Node.
data NodeParams = NodeParams
    { npDbPathM       :: !FilePath          -- ^ Path to node's database.
    , npRebuildDb     :: !Bool              -- ^ @True@ if data-base should be rebuilt
    , npSystemStart   :: !Timestamp         -- ^ System start
    , npSecretKey     :: !SecretKey         -- ^ Primary secret key of node
    , npUserSecret    :: !UserSecret        -- ^ All node secret keys
    , npBaseParams    :: !BaseParams        -- ^ See 'BaseParams'
    , npCustomUtxo    :: !Utxo              -- ^ predefined custom utxo
    , npTimeLord      :: !Bool              -- ^ @True@ if node started as time-lord
    , npJLFile        :: !(Maybe FilePath)  -- @georgeee please write comment to this field when you see this sign, i made it very long on purpose so it won't fit even in your huge monitor
    , npAttackTypes   :: ![AttackType]      -- ^ List of attack types used by malicious emulation
    , npAttackTargets :: ![AttackTarget]    -- ^ List of targets to attack by malicious emulation
    , npPropagation   :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , npReportServers :: ![Text]            -- ^ List of report server URLs
    , npUpdateParams  :: !UpdateParams      -- ^ Params for update system
    } deriving (Show)
