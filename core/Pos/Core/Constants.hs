{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Constants that the rest of the code needs to know. They're available
-- from this module directly, instead of being passed as a config.
module Pos.Core.Constants
       ( epochSlots
       , blkSecurityParam
       , slotSecurityParam
       , isDevelopment
       , protocolMagic
       , staticSysStart
       ) where

import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Serokell.Util              (sec)
import           Universum                  hiding (lift)

#ifdef DEV_MODE
import           System.IO.Unsafe           (unsafePerformIO)
#else
import           Language.Haskell.TH.Syntax (lift, runIO)
#endif

import           Pos.Core.Constants.Type    (CoreConstants (..))
import           Pos.Core.Timestamp         (Timestamp (..))
import           Pos.Util.Config            (configFilePath, unsafeReadConfig)

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

coreConstants :: CoreConstants
#ifdef DEV_MODE
coreConstants = unsafePerformIO (unsafeReadConfig =<< configFilePath)
{-# NOINLINE coreConstants #-}
#else
coreConstants = $(do
    x :: CoreConstants <- runIO (unsafeReadConfig =<< configFilePath)
    lift x)
#endif

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
isDevelopment = isNothing staticSysStart

-- | System start time embeded into binary.
staticSysStart :: Maybe Timestamp
#ifdef DEV_MODE
staticSysStart = Nothing
  where
    -- to avoid the “defined but not used” warning
    _ = ccProductionNetworkStartTime
#else
staticSysStart = Just $ Timestamp $ sec $
    let st = ccProductionNetworkStartTime coreConstants
    in if st > 0 then st
       else let pause = 30
                divider = 10
                after3Mins = pause + unsafePerformIO (round <$> getPOSIXTime)
                minuteMod = after3Mins `mod` divider
                alignment = if minuteMod > (divider `div` 2) then 1 else 0
            in (after3Mins `div` divider + alignment) * divider
               -- ^ If several local nodes are started within 20 sec,
               -- they'll have same start time
#endif

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ coreConstants
