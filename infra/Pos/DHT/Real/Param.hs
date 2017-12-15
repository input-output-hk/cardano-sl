module Pos.DHT.Real.Param
       ( KademliaParams (..)
       , fromYamlConfig
       , MalformedDHTKey (..)
       ) where

import qualified Data.ByteString.Base64.URL as B64 (decode)
import qualified Data.ByteString.Char8 as B8
import           Pos.DHT.Model.Types (DHTKey, bytesToDHTKey)
import qualified Pos.Network.Yaml as Y
import           Pos.Util.TimeWarp (NetworkAddress)
import           Universum

-- | Parameters for the Kademlia DHT subsystem.
data KademliaParams = KademliaParams
    { kpNetworkAddress  :: !(Maybe NetworkAddress)
    , kpPeers           :: ![NetworkAddress]
    -- ^ Peers passed from CLI
    , kpKey             :: !(Maybe DHTKey)
    , kpExplicitInitial :: !Bool
    , kpDumpFile        :: !(Maybe FilePath)
    -- ^ Path to kademlia dump file
    , kpExternalAddress :: !(Maybe NetworkAddress)
    -- ^ External address of node
    } deriving (Show)

-- | Get a KademliaParams from its yaml counterpart. Could fail because the
-- DHTKey must be a base64-url encoded valid DHTKey.
fromYamlConfig :: Y.KademliaParams -> Either String KademliaParams
fromYamlConfig yamlParams = do
    parsedKey <- case Y.kpId yamlParams of
        Nothing  -> pure Nothing
        Just key -> Just <$> kademliaIdToDHTKey key
    return $ KademliaParams
        { kpKey             = parsedKey
        , kpNetworkAddress  = kademliaAddressToNetworkAddress <$> Y.kpBind yamlParams
        , kpExternalAddress = kademliaAddressToNetworkAddress <$> Y.kpAddress yamlParams
        , kpPeers           = kademliaAddressToNetworkAddress <$> Y.kpPeers yamlParams
        , kpDumpFile        = Y.kpDumpFile yamlParams
        , kpExplicitInitial = fromMaybe False (Y.kpExplicitInitial yamlParams)
        }

kademliaAddressToNetworkAddress :: Y.KademliaAddress -> NetworkAddress
kademliaAddressToNetworkAddress yamlAddr =
    (B8.pack $ Y.kaHost yamlAddr, Y.kaPort yamlAddr)

kademliaIdToDHTKey :: Y.KademliaId -> Either String DHTKey
kademliaIdToDHTKey (Y.KademliaId txt) = do
    bytes <- B64.decode (B8.pack txt)
    bytesToDHTKey bytes

data MalformedDHTKey = MalformedDHTKey String
    deriving (Show, Typeable)

instance Exception MalformedDHTKey
