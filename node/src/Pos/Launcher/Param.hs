{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , TransportParams (..)
       , NodeParams (..)
       ) where

import           Universum

import           Control.Lens                (makeLensesWith)
import           Ether.Internal              (HasLens (..))
import qualified Network.Transport.TCP       as TCP
import           System.Wlog                 (LoggerName)

import           Pos.Behavior                (BehaviorConfig (..))
import           Pos.Core                    (HasPrimaryKey (..))
import           Pos.Crypto                  (SecretKey)
import           Pos.DHT.Real.Param          (KademliaParams)
import           Pos.Network.Types           (NetworkConfig)
import           Pos.Reporting.MemState      (HasReportServers (..))
import           Pos.Security.Params         (SecurityParams)
import           Pos.Ssc.GodTossing.Behavior (GtBehavior)
import           Pos.Statistics              (EkgParams, StatsdParams)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.UserSecret         (UserSecret)
import           Pos.Util.Util               (postfixLFields)

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

-- | Network parameters.
data TransportParams = TransportParams
    { tpTcpAddr   :: !TCP.TCPAddr
    -- ^ External TCP address of the node.
    -- It encapsulates bind address and address visible to other nodes.
    }

-- | This data type contains all data necessary to launch node and
-- known in advance (from CLI, configs, etc.)
data NodeParams = NodeParams
    { npDbPathM        :: !FilePath             -- ^ Path to node's database
    , npRebuildDb      :: !Bool                 -- ^ @True@ if data-base should be rebuilt
    , npSecretKey      :: !SecretKey            -- ^ Primary secret key of node
    , npUserSecret     :: !UserSecret           -- ^ All node secret keys
    , npBaseParams     :: !BaseParams           -- ^ See 'BaseParams'
    , npJLFile         :: !(Maybe FilePath)     -- TODO COMMENT
    , npReportServers  :: ![Text]               -- ^ List of report server URLs
    , npUpdateParams   :: !UpdateParams         -- ^ Params for update system
    , npUseNTP         :: !Bool                 -- ^ Whether to use synchronisation with NTP servers.
    , npTransport      :: !TransportParams      -- ^ (TCP) transport parameters.
    , npEnableMetrics  :: !Bool                 -- ^ Gather runtime statistics.
    , npEkgParams      :: !(Maybe EkgParams)    -- ^ EKG statistics monitoring.
    , npStatsdParams   :: !(Maybe StatsdParams) -- ^ statsd statistics backend.
    , npNetworkConfig  :: !(NetworkConfig KademliaParams)
    , npBehaviorConfig :: !BehaviorConfig       -- ^ Behavior (e.g. GodTossing settings)
    } -- deriving (Show)

makeLensesWith postfixLFields ''NodeParams
makeLensesWith postfixLFields ''BehaviorConfig

instance HasLens UpdateParams NodeParams UpdateParams where
    lensOf = npUpdateParams_L

instance HasLens BehaviorConfig NodeParams BehaviorConfig where
    lensOf = npBehaviorConfig_L
instance HasLens SecurityParams NodeParams SecurityParams where
    lensOf = npBehaviorConfig_L . bcSecurityParams_L
instance HasLens GtBehavior NodeParams GtBehavior where
    lensOf = npBehaviorConfig_L . bcGtBehavior_L

instance HasLens (NetworkConfig KademliaParams) NodeParams (NetworkConfig KademliaParams) where
    lensOf = npNetworkConfig_L

instance HasReportServers NodeParams where
    reportServers = npReportServers_L

instance HasPrimaryKey NodeParams where
    primaryKey = npSecretKey_L
