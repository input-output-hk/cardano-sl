{-# LANGUAGE CPP #-}

{-| Constants used by algorithm. See paper for more details.
    Some constants are parsed at compile-time (see 'Pos.CompileConfig.Type').
    Others are derived from those.
-}

module Pos.Constants
       (
         module Pos.Core.Constants
       , module Pos.Core.Genesis
       , module Pos.DHT.Constants
       , module Pos.Communication.Constants
       , module Pos.Slotting.Constants
       , module Pos.Update.Constants
       , module Pos.Ssc.GodTossing.Constants

       -- * Constants mentioned in paper
       , networkDiameter

       -- * Other constants
       , networkConnectionTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers
       , recoveryHeadersMessage
       , messageCacheTimeout

       -- * Delegation
       , lightDlgConfirmationTimeout
       , dlgCacheParam

       -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold

       -- * Transaction resubmition constants
       , pendingTxResubmitionPeriod
       ) where

import           Universum

import           Data.Time.Units              (Microsecond, Second)
import           Serokell.Util                (ms, sec)
import qualified Text.Parsec                  as P

import           Pos.CompileConfig            (CompileConfig (..), compileConfig)
import           Pos.Util                     ()
import           Pos.Util.TimeWarp            (NetworkAddress, addrParser)

-- Reexports
import           Pos.Communication.Constants
import           Pos.Core.Constants
import           Pos.Core.Genesis
import           Pos.DHT.Constants
import           Pos.Slotting.Constants
import           Pos.Ssc.GodTossing.Constants
import           Pos.Update.Constants

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.CompileConfig.ccNetworkDiameter'.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

networkConnectionTimeout :: Microsecond
networkConnectionTimeout = ms . fromIntegral . ccNetworkConnectionTimeout $ compileConfig

blockRetrievalQueueSize :: Integral a => a
blockRetrievalQueueSize =
    fromIntegral . ccBlockRetrievalQueueSize $ compileConfig

propagationQueueSize :: Integral a => a
propagationQueueSize =
    fromIntegral $ ccPropagationQueueSize $ compileConfig

-- | See 'Pos.CompileConfig.ccDefaultPeers'.
defaultPeers :: [NetworkAddress]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: Text -> NetworkAddress
    parsePeer =
        either (error . show) identity .
        P.parse addrParser "Compile time config"

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ compileConfig

-- | Timeout for caching system. Components that use caching on
-- messages can use this timeout to invalidate caches.
messageCacheTimeout :: (Integral a) => a
messageCacheTimeout = fromIntegral . ccMessageCacheTimeout $ compileConfig

----------------------------------------------------------------------------
-- Delegation
----------------------------------------------------------------------------

-- | Amount of time we hold confirmations for light PSKs.
lightDlgConfirmationTimeout :: (Integral a) => a
lightDlgConfirmationTimeout = fromIntegral . ccLightDlgConfirmationTimeout $ compileConfig

-- | This value parameterizes size of cache used in Delegation.
-- Not bytes, but number of elements.
dlgCacheParam :: Integral n => n
dlgCacheParam = fromIntegral . ccDlgCacheParam $ compileConfig

----------------------------------------------------------------------------
-- Malicious activity
----------------------------------------------------------------------------

-- | Number of slots used by malicious actions detection to check if
-- we are not receiving generated blocks.
mdNoBlocksSlotThreshold :: Integral i => i
mdNoBlocksSlotThreshold = fromIntegral . ccMdNoBlocksSlotThreshold $ compileConfig

----------------------------------------------------------------------------
-- Transactions resubmition
----------------------------------------------------------------------------

pendingTxResubmitionPeriod :: Second
pendingTxResubmitionPeriod = fromIntegral . ccPendingTxResubmissionPeriod $ compileConfig
