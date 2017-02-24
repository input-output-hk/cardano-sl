{-# LANGUAGE UndecidableInstances #-}

-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.TimeWarp
       ( NetworkAddress
       , localhost

       , currentTime
       , mcs
       , ms
       , sec
       , minute
       , hour
       , addressToNodeId
       , addressToNodeId'
       , nodeIdToAddress
       ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Char             (isNumber)
import           Data.Time.Units       (Microsecond)
import           Data.Time.Units       (fromMicroseconds)
import           Mockable              (realTime)
import qualified Network.Transport.TCP as TCP
import           Node                  (NodeId (..))
import           Prelude               (read)
import           Universum

-- | @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | Full node address.
type NetworkAddress = (ByteString, Word16)

-- | Temporal solution
currentTime :: MonadIO m => m Microsecond
currentTime = realTime

-- | Converts a specified time to `Microsecond`.
mcs, ms, sec, minute, hour :: Int -> Microsecond
mcs    = fromMicroseconds . fromIntegral
ms     = fromMicroseconds . fromIntegral . (*) 1000
sec    = fromMicroseconds . fromIntegral . (*) 1000000
minute = fromMicroseconds . fromIntegral . (*) 60000000
hour   = fromMicroseconds . fromIntegral . (*) 3600000000

-- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
addressToNodeId :: NetworkAddress -> NodeId
addressToNodeId = addressToNodeId' 0

addressToNodeId' :: Word32 -> NetworkAddress -> NodeId
addressToNodeId' eId (host, port) =
    NodeId $ TCP.encodeEndPointAddress (BS8.unpack host) (show port) eId

nodeIdToAddress :: NodeId -> Maybe NetworkAddress
nodeIdToAddress (NodeId ep) = toNA =<< TCP.decodeEndPointAddress ep
  where
    toNA (hostName, port', _) = (BS8.pack hostName,) <$> toPort port'
    toPort :: String -> Maybe Word16
    toPort port' | all isNumber port' = pure $ read port'
                 | otherwise          = Nothing
