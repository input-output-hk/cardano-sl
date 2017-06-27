{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NetworkParams (..)
       , NodeParams (..)
       , GenesisUtxo(..)
       ) where

import           Universum

import           Control.Lens            (coerced, makeLensesWith)
import           Ether.Internal          (HasLens (..))
import qualified Network.Transport.TCP   as TCP
import           System.Wlog             (LoggerName)

import           Pos.Communication.Relay (HasPropagationFlag (..))
import           Pos.Communication.Types (NodeId)
import           Pos.Core                (GenesisStakes (..), HasPrimaryKey (..),
                                          StakesMap, Timestamp)
import           Pos.Crypto              (SecretKey)
import           Pos.DHT.Real            (KademliaParams)
import           Pos.Reporting.MemState  (HasReportServers (..))
import           Pos.Security.Params     (SecurityParams)
import           Pos.Txp.Toil.Types      (Utxo)
import           Pos.Update.Params       (UpdateParams)
import           Pos.Util.UserSecret     (UserSecret)
import           Pos.Util.Util           (postfixLFields)

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

newtype GenesisUtxo = GenesisUtxo { unGenesisUtxo :: Utxo }

-- | This data type contains all data necessary to launch node and
-- known in advance (from CLI, configs, etc.)
data NodeParams = NodeParams
    { npDbPathM        :: !FilePath          -- ^ Path to node's database.
    , npRebuildDb      :: !Bool              -- ^ @True@ if data-base should be rebuilt
    , npSystemStart    :: !Timestamp         -- ^ System start
    , npSecretKey      :: !SecretKey         -- ^ Primary secret key of node
    , npUserSecret     :: !UserSecret        -- ^ All node secret keys
    , npBaseParams     :: !BaseParams        -- ^ See 'BaseParams'
    , npCustomUtxo     :: !Utxo              -- ^ Predefined genesis utxo.
    , npGenesisStakes  :: !StakesMap         -- ^ Predefined genesis stakes.
    , npJLFile         :: !(Maybe FilePath)  -- @georgeee please write comment to this field when you see this sign, i made it very long on purpose so it won't fit even in your huge monitor
    , npPropagation    :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , npReportServers  :: ![Text]            -- ^ List of report server URLs
    , npUpdateParams   :: !UpdateParams      -- ^ Params for update system
    , npSecurityParams :: !SecurityParams    -- ^ Params for "Pos.Security"
    , npUseNTP         :: !Bool
    , npNetwork        :: !NetworkParams
    -- ^ Network parameters.
    } -- deriving (Show)

makeLensesWith postfixLFields ''NodeParams

instance HasLens UpdateParams NodeParams UpdateParams where
    lensOf = npUpdateParams_L

instance HasLens SecurityParams NodeParams SecurityParams where
    lensOf = npSecurityParams_L

instance HasLens GenesisUtxo NodeParams GenesisUtxo where
    lensOf = npCustomUtxo_L . coerced

instance HasLens GenesisStakes NodeParams GenesisStakes where
    lensOf = npGenesisStakes_L . coerced

instance HasReportServers NodeParams where
    reportServers = npReportServers_L

instance HasPropagationFlag NodeParams where
    propagationFlag = npPropagation_L

instance HasPrimaryKey NodeParams where
    primaryKey = npSecretKey_L
