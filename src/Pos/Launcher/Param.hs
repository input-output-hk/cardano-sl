-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       ) where

import           Universum

import           System.Wlog         (LoggerName)

import           Pos.Crypto          (SecretKey)
import           Pos.Security.Params (SecurityParams)
import           Pos.Statistics      (EkgParams, StatsdParams)
import           Pos.Txp.Toil.Types  (Utxo)
import           Pos.Types           (Timestamp)
import           Pos.Update.Params   (UpdateParams)
import           Pos.Util.UserSecret (UserSecret)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName        -- ^ Prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)  -- ^ Prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)  -- ^ Path to logger configuration
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpLoggingParams   :: !LoggingParams  -- ^ Logger parameters
    } deriving (Show)

-- | Contains algorithm specific & storage parameters for Node.
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
    , npEnableMetrics :: !Bool              -- ^ Gather runtime statistics.
    , npEkgParams     :: !(Maybe EkgParams) -- ^ EKG statistics monitoring.
    , npStatsdParams  :: !(Maybe StatsdParams) -- ^ statsd statistics backend.
    } deriving (Show)
