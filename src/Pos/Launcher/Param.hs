-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       ) where

import           System.Wlog (LoggerName)
import           Universum

import           Pos.Crypto  (SecretKey, VssKeyPair)
import           Pos.DHT     (DHTKey, DHTNode, DHTNodeType)
import           Pos.Types   (Timestamp, Utxo)

data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName  -- ^ prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)
    , lpConfigPath    :: !(Maybe FilePath)
    } deriving (Show)

data BaseParams = BaseParams
    { bpPort               :: !Word16
    , bpDHTPeers           :: ![DHTNode]
    , bpDHTKeyOrType       :: !(Either DHTKey DHTNodeType)
    , bpDHTExplicitInitial :: !Bool
    , bpLoggingParams      :: !LoggingParams
    } deriving (Show)

data NodeParams = NodeParams
    { npDbPath      :: !(Maybe FilePath)
    , npRebuildDb   :: !Bool
    , npSystemStart :: !Timestamp
    , npSecretKey   :: !SecretKey
    , npVssKeyPair  :: !VssKeyPair
    , npBaseParams  :: !BaseParams
    , npCustomUtxo  :: !(Maybe Utxo)
    , npTimeLord    :: !Bool
    , npJLFile      :: !(Maybe FilePath)
    } deriving (Show)
