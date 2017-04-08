{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Constants that the rest of the code needs to know. They're available
-- from this module directly, instead of being passed as a config.
module Pos.Core.Constants
       ( CoreConstants(..)
       , coreConstants

       , epochSlots
       , blkSecurityParam
       , slotSecurityParam
       , isDevelopment
       , protocolMagic
       , staticSysStart
       ) where

import           Data.Aeson             (FromJSON (..), genericParseJSON)
import           Data.Tagged            (Tagged (..))
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util          (sec)
import           System.IO.Unsafe       (unsafePerformIO)
import           Universum

import           Pos.Core.Timestamp     (Timestamp (..))
import           Pos.Util.Config        (IsConfig (..), configParser, parseFromCslConfig)

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
staticSysStart :: Maybe Timestamp
staticSysStart
    | isDevelopment = Nothing
    | st > 0        = Just $ Timestamp $ sec st
    -- If several local nodes are started within 20 sec,
    -- they'll have same start time
    | otherwise     = Just $ Timestamp $ sec $
          (after3Mins `div` divider + alignment) * divider
  where
    st = ccProductionNetworkStartTime coreConstants
    pause = 30
    divider = 20
    after3Mins :: Int
    after3Mins = pause + unsafePerformIO (round <$> getPOSIXTime)
    minuteMod = after3Mins `mod` divider
    alignment = if minuteMod > (divider `div` 2) then 1 else 0

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ coreConstants
