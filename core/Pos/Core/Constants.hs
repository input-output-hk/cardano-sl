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

#ifndef DEV_MODE
import           Language.Haskell.TH.Syntax (lift, runIO)
#endif

import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Serokell.Util              (sec)
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum                  hiding (lift)

import           Pos.Core.Constants.Type    (CoreConstants (..))
import           Pos.Core.Timestamp         (Timestamp (..))
import           Pos.Util.Config            (cslConfigFilePath, unsafeReadConfig)

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

coreConstants :: CoreConstants
#ifdef DEV_MODE
coreConstants = unsafePerformIO (unsafeReadConfig =<< cslConfigFilePath)
{-# NOINLINE coreConstants #-}
#else
coreConstants = $(do
    x :: CoreConstants <- runIO (unsafeReadConfig =<< cslConfigFilePath)
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
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif

-- | System start time embeded into binary.
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
