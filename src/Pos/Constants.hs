{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Constants used by algorithm. See paper for more details.
    Some constants are parsed at compile-time (see 'Pos.CompileConfig.Type').
    Others are derived from those.
-}

module Pos.Constants
       ( k
       , slotDuration
       , epochSlots
       , epochDuration
       , sharedSeedLength
       , genesisN
       , maxLocalTxs
       , neighborsSendThreshold
       , networkDiameter
       , RunningMode (..)
       , runningMode
       , isDevelopment
       , defaultPeers
       , sysTimeBroadcastSlots
       , mpcSendInterval
       ) where

import           Control.TimeWarp.Timed (Microsecond, sec)
import qualified Text.Parsec            as P
import           Universum

import           Pos.CLI                (dhtNodeParser)
import           Pos.CompileConfig      (CompileConfig (..), compileConfig)
import           Pos.DHT.Types          (DHTNode)
import           Pos.Types.Timestamp    (Timestamp)

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = fromIntegral . ccK $ compileConfig

-- | Length of slot. Also see 'Pos.CompileConfig.ccSlotDurationSec'.
slotDuration :: Microsecond
slotDuration = sec . ccSlotDurationSec $ compileConfig

-- | Number of slots inside one epoch.
epochSlots :: Integral a => a
epochSlots = 6 * k

-- | Length of one epoch in 'Microsecond's.
epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | See 'Pos.CompileConfig.ccGenesisN'.
genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ compileConfig

-- | Maximum amount of transactions we have in storage
-- (i.e. we can accept without putting them in block).
-- There're next kind of storages in our implementation:
--
-- * temporary storage of transactions
--
-- * utxo map that corresponds to it
--
-- * utxo of blocks in history
--
-- This constant is size of first set.
-- Also see 'Pos.CompileConfig.ccMaxLocalTxs'.
maxLocalTxs :: Integral i => i
maxLocalTxs = fromIntegral . ccMaxLocalTxs $ compileConfig

-- | /Time-lord/ node announces system start time by broadcast. She does it
-- during first 'Pos.CompileConfig.ccSysTimeBroadcastSlots' slots.
sysTimeBroadcastSlots :: Integral i => i
sysTimeBroadcastSlots = fromIntegral . ccSysTimeBroadcastSlots $ compileConfig

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.CompileConfig.ccNetworkDiameter'.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ compileConfig

-- | Defines mode of running application: in tested mode or in production.
data RunningMode
    = Development
    | Production { rmSystemStart :: !Timestamp}

-- | Current running mode.
runningMode :: RunningMode
#ifdef DEV_MODE
runningMode = Development
#else
runningMode = Production $ panic "System start is not known!"
#endif

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
isDevelopment = case runningMode of
                  Development -> True
                  _           -> False

-- | See 'Pos.CompileConfig.ccDefaultPeers'.
defaultPeers :: [DHTNode]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: [Char] -> DHTNode
    parsePeer =
        either (panic . show) identity .
        P.parse dhtNodeParser "Compile time config"

-- | Length of interval during which node should send her MPC
-- message. Relevant only for one SSC implementation.
-- Also see 'Pos.CompileConfig.ccMpcSendInterval'.
mpcSendInterval :: Microsecond
mpcSendInterval = sec . fromIntegral . ccMpcSendInterval $ compileConfig
