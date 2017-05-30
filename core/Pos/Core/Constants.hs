{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Constants that the rest of the code needs to know. They're
-- available from this module directly, instead of being passed as a
-- config. Also some constants aren't configurable.

module Pos.Core.Constants
       (
       -- * Non-configurable constants
         sharedSeedLength
       , genesisHash

       -- * The config structure
       , CoreConstants(..)
       , coreConstants

       -- * Constants
       , epochSlots
       , blkSecurityParam
       , slotSecurityParam
       , isDevelopment
       , protocolMagic
       , staticSysStart
       , memPoolLimitRatio
       -- * Genesis constants
       , genesisN
       ) where

import           Data.Aeson             (FromJSON (..), genericParseJSON)
import           Data.Tagged            (Tagged (..))
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util          (sec)
import           Universum

import           Pos.Core.Timestamp     (Timestamp (..))
import           Pos.Crypto.Hashing     (Hash, unsafeHash)
import           Pos.Util.Config        (IsConfig (..), configParser, parseFromCslConfig)

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

coreConstants :: CoreConstants
coreConstants =
    case parseFromCslConfig configParser of
        Left err -> error (toText ("Couldn't parse core config: " ++ err))
        Right x  -> x

data CoreConstants = CoreConstants
    {
      -- | Security parameter from paper
      ccK                          :: !Int
    , -- | Magic constant for separating real/testnet
      ccProtocolMagic              :: !Int32
    , -- | Start time of network (in @Production@ running mode). If set to
      -- zero, then running time is 2 minutes after build.
      ccProductionNetworkStartTime :: !Int
    , -- | Number of pre-generated keys
      ccGenesisN                   :: !Int
      -- | Size of mem pool will be limited by this value muliplied by block
      -- size limit.
    , ccMemPoolLimitRatio          :: !Word
    }
    deriving (Show, Generic)

instance FromJSON CoreConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig CoreConstants where
    configPrefix = Tagged Nothing

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
blkSecurityParam :: Integral a => a
blkSecurityParam = fromIntegral . ccK $ coreConstants

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chain_quality@.
slotSecurityParam :: Integral a => a
slotSecurityParam = 2 * blkSecurityParam

-- | Number of slots inside one epoch.
epochSlots :: Integral a => a
epochSlots = 10 * blkSecurityParam

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif

-- | System start time embedded into binary.
staticSysStart :: Timestamp
staticSysStart
    | isDevelopment = error "System start time should be passed \
                              \as a command line argument in dev mode."
    | otherwise     =
          Timestamp $ sec $ ccProductionNetworkStartTime coreConstants

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ coreConstants

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | Number of pre-generated keys
genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ coreConstants

----------------------------------------------------------------------------
-- Hardware/system
----------------------------------------------------------------------------

-- | Size of mem pool will be limited by this value muliplied by block
-- size limit.
memPoolLimitRatio :: Integral i => i
memPoolLimitRatio = fromIntegral . ccMemPoolLimitRatio $ coreConstants
