{-# LANGUAGE CPP #-}

-- | Raw constants which we want to have in 'core'. They have simpler
-- types than constants from 'Typed' module. It's done to avoid cyclic
-- dependencies. To be more precise, this module doesn't import
-- 'Pos.Core.Types', while 'Typed' module does.

module Pos.Core.Constants.Raw
       (
       -- * Non-configurable constants
         sharedSeedLength
       , genesisHash

       -- * The config structure
       , CoreConfig(..)
       , coreConfig

       -- * Constants
       , isDevelopment
       , dbSerializeVersion
       , protocolMagic
       , memPoolLimitRatio

       , genesisBinSuffix

       , nonCriticalCQBootstrap
       , criticalCQBootstrap
       , nonCriticalCQ
       , criticalCQ
       , criticalForkThreshold
       , fixedTimeCQ
       , fixedTimeCQSec
       ) where

import           Universum

import           Data.Aeson             (FromJSON (..), genericParseJSON)
import qualified Data.Aeson.Types       as A
import           Data.Tagged            (Tagged (..))
import           Data.Time.Units        (Microsecond, Second, convertUnit)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util          (sec)

import           Pos.Crypto.Hashing     (Hash, unsafeHash)
import           Pos.Util.Config        (IsConfig (..), configParser, parseFromCslConfig)
import           Pos.Util.Util          ()

----------------------------------------------------------------------------
-- Constants which are not configurable
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | Predefined 'Hash' of the parent of the 0-th genesis block (which
-- is the only block without a real parent).
genesisHash :: Hash a
genesisHash = unsafeHash @Text "patak"
{-# INLINE genesisHash #-}

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

data CoreConfig = CoreConfig
    {
      -- | Security parameter from paper
      ccK                      :: !Int
    , -- | Versioning for values in node's DB
      ccDbSerializeVersion     :: Word8
    , -- | Magic constant for separating real/testnet
      ccProtocolMagic          :: !Int32
      -- | Size of mem pool will be limited by this value muliplied by block
      -- size limit.
    , ccMemPoolLimitRatio      :: !Word
      -- | Suffix for genesis.bin files
    , ccGenesisBinSuffix       :: ![Char]

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
    , ccFixedTimeCQ            :: !Int
    }
    deriving (Show, Generic)

-- | Suffix for genesis.bin files
genesisBinSuffix :: String
genesisBinSuffix = ccGenesisBinSuffix coreConfig

coreConfig :: CoreConfig
coreConfig =
    case parseFromCslConfig configParser of
        Left err -> error (toText ("Couldn't parse core config: " ++ err))
        Right x  -> x

instance FromJSON CoreConfig where
    parseJSON = checkConstants <=< genericParseJSON defaultOptions

instance IsConfig CoreConfig where
    configPrefix = Tagged Nothing

-- | Check invariants.
--
-- Note: during genesis files rework (CSL-1617) all these values were
-- moved into 'GenesisSpec', so currently there are no checks.
checkConstants :: CoreConfig -> A.Parser CoreConfig
checkConstants = pure
-- checkConstants cs@CoreConfig{..} = do
--     let check :: [a -> Bool] -> a -> Bool
--         check ps x = all ($ x) ps
--     unless (check [(>= 0), (< 1)] ccGenesisMpcThd) $
--         fail "CoreConfig: genesisMpcThd is not in range [0, 1)"
--     unless (check [(>= 0), (< 1)] ccGenesisUpdateVoteThd) $
--         fail "CoreConfig: genesisUpdateVoteThd is not in range [0, 1)"
--     unless (check [(>= 0), (< 1)] ccGenesisHeavyDelThd) $
--         fail "CoreConfig: genesisHeavyDelThd is not in range [0, 1)"
--     unless (check [(> 0), (< 1)] ccGenesisUpdateProposalThd) $
--         fail "CoreConfig: genesisUpdateProposalThd is not in range (0, 1)"
--     unless (check [(> 0), (< 1)] ccGenesisSoftforkInit) $
--         fail "CoreConfig: genesisSoftforkInit is not in range (0, 1)"
--     unless (check [(> 0), (< 1)] ccGenesisSoftforkMin) $
--         fail "CoreConfig: genesisSoftforkMin is not in range (0, 1)"
--     unless (check [(> 0), (< 1)] ccGenesisSoftforkDec) $
--         fail "CoreConfig: genesisSoftforkDec is not in range (0, 1)"
--     pure cs

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif

-- | DB format version. When serializing items into the node's DB, the values are paired
-- with this constant.
dbSerializeVersion :: Word8
dbSerializeVersion = fromIntegral . ccDbSerializeVersion $ coreConfig

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ coreConfig

-- | Size of mem pool will be limited by this value muliplied by block
-- size limit.
memPoolLimitRatio :: Integral i => i
memPoolLimitRatio = fromIntegral . ccMemPoolLimitRatio $ coreConfig

-- | If chain quality in bootstrap era is less than this value,
-- non critical misbehavior will be reported.
nonCriticalCQBootstrap :: Double
nonCriticalCQBootstrap = ccNonCriticalCQBootstrap coreConfig

-- | If chain quality in bootstrap era is less than this value,
-- critical misbehavior will be reported.
criticalCQBootstrap :: Double
criticalCQBootstrap = ccCriticalCQBootstrap coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, non critical misbehavior will be reported.
nonCriticalCQ :: Double
nonCriticalCQ = ccNonCriticalCQ coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalCQ :: Double
criticalCQ = ccCriticalCQ coreConfig

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalForkThreshold :: Integral i => i
criticalForkThreshold = fromIntegral . ccCriticalForkThreshold $ coreConfig

-- | Chain quality will be also calculated for this amount of time.
fixedTimeCQ :: Microsecond
fixedTimeCQ = sec . ccFixedTimeCQ $ coreConfig

-- | 'fixedTimeCQ' expressed as seconds.
fixedTimeCQSec :: Second
fixedTimeCQSec = convertUnit fixedTimeCQ
