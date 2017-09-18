{-# LANGUAGE Rank2Types #-}

-- | Flobal constants, configurable via Data.Reflection.

module Pos.Core.Configuration.Core
       (
       -- * The configuration structure
         CoreConfiguration(..)
       , GenesisConfiguration(..)

       , HasCoreConfiguration
       , withCoreConfiguration

       , coreConfiguration
       , dbSerializeVersion
       , memPoolLimitRatio
       , nonCriticalCQBootstrap
       , criticalCQBootstrap
       , nonCriticalCQ
       , criticalCQ
       , criticalForkThreshold
       , fixedTimeCQ
       , fixedTimeCQSec
       , webLoggingEnabled

       ) where

import           Universum

import           Data.Reflection        (Given (..), give)
import           Data.Time.Units        (Microsecond, Second, convertUnit)

import           Pos.Core.Genesis.Types (GenesisSpec (..))

data GenesisConfiguration =
      -- | Genesis from a 'GenesisSpec'.
      GCSpec !GenesisSpec
      -- | 'GenesisData' in canonical JSON at this location.
    | GCSrc !FilePath
    deriving (Show)

data CoreConfiguration = CoreConfiguration
    {
      -- | Specifies the genesis
      ccGenesis                :: !GenesisConfiguration

    , -- | Versioning for values in node's DB
      ccDbSerializeVersion     :: !Word8
      -- | Size of mem pool will be limited by this value muliplied by block
      -- size limit.
    , ccMemPoolLimitRatio      :: !Word

      -- Chain quality thresholds and other constants to detect
      -- suspicious things.

      -- | If chain quality in bootstrap era is less than this value,
      -- non critical misbehavior will be reported.
    , ccNonCriticalCQBootstrap :: !Double
      -- | If chain quality in bootstrap era is less than this value,
      -- critical misbehavior will be reported.
    , ccCriticalCQBootstrap    :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, non critical misbehavior will be reported.
    , ccNonCriticalCQ          :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, critical misbehavior will be reported.
    , ccCriticalCQ             :: !Double
      -- | Number of blocks such that if so many blocks are rolled
      -- back, it requires immediate reaction.
    , ccCriticalForkThreshold  :: !Int
      -- | Chain quality will be also calculated for this amount of seconds.
    , ccFixedTimeCQ            :: !Microsecond

      -- Web settings

      -- | Whether incoming requests logging should be performed by web
      -- part
    , ccWebLoggingEnabled      :: !Bool
    }
    deriving (Show, Generic)

type HasCoreConfiguration = Given CoreConfiguration

withCoreConfiguration :: CoreConfiguration -> (HasCoreConfiguration => r) -> r
withCoreConfiguration = give

coreConfiguration :: HasCoreConfiguration => CoreConfiguration
coreConfiguration = given

-- | DB format version. When serializing items into the node's DB, the values are paired
-- with this constant.
dbSerializeVersion :: HasCoreConfiguration => Word8
dbSerializeVersion = fromIntegral . ccDbSerializeVersion $ coreConfiguration

-- | Size of mem pool will be limited by this value muliplied by block
-- size limit.
memPoolLimitRatio :: (HasCoreConfiguration, Integral i) => i
memPoolLimitRatio = fromIntegral . ccMemPoolLimitRatio $ coreConfiguration

-- | If chain quality in bootstrap era is less than this value,
-- non critical misbehavior will be reported.
nonCriticalCQBootstrap :: HasCoreConfiguration => Double
nonCriticalCQBootstrap = ccNonCriticalCQBootstrap coreConfiguration

-- | If chain quality in bootstrap era is less than this value,
-- critical misbehavior will be reported.
criticalCQBootstrap :: HasCoreConfiguration => Double
criticalCQBootstrap = ccCriticalCQBootstrap coreConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, non critical misbehavior will be reported.
nonCriticalCQ :: HasCoreConfiguration => Double
nonCriticalCQ = ccNonCriticalCQ coreConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalCQ :: HasCoreConfiguration => Double
criticalCQ = ccCriticalCQ coreConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalForkThreshold :: (HasCoreConfiguration, Integral i) => i
criticalForkThreshold = fromIntegral . ccCriticalForkThreshold $ coreConfiguration

-- | Chain quality will be also calculated for this amount of time.
fixedTimeCQ :: HasCoreConfiguration => Microsecond
fixedTimeCQ = ccFixedTimeCQ coreConfiguration

-- | 'fixedTimeCQ' expressed as seconds.
fixedTimeCQSec :: HasCoreConfiguration => Second
fixedTimeCQSec = convertUnit fixedTimeCQ

-- | Web logging might be disabled for security concerns.
webLoggingEnabled :: HasCoreConfiguration => Bool
webLoggingEnabled = ccWebLoggingEnabled coreConfiguration
