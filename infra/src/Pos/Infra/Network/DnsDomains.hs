{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Infra.Network.DnsDomains
       ( DnsDomains(..)
       , NodeAddr(..)
       , resolveDnsDomains
       , extractNodeAddr
       ) where


import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.HashMap.Strict as HMS
import           Data.IP (IP (..), IPv4)
import           Data.IP.Internal (IPv4 (..), IPv6 (..))
import qualified Data.Text as T
import           Network.Broadcast.OutboundQueue.Types (AllOf, Alts)
import qualified Network.DNS as DNS

import           Pos.Util.Util (aesonError, toAesonError)

-- | DNS domains for relay discovery
--
-- We provide a list of list of domain names to query.
-- Just like for static routes, it's a conjunction of disjuctions:
-- try to use one alternative from each list. This implicitly encodes
-- valency and fallbacks.
-- The idea is that resolving at least one name from each of the inner lists
-- provides a complete set of relay nodes to be tried, using the
-- backup domain names (further back in the inner lists) only when one or more
-- of the primary ones (further forward in the inner lists) fail.
data DnsDomains a = DnsDomains {
      dnsDomains :: !(AllOf (Alts (NodeAddr a)))
    }
  deriving (Show, Eq, Generic)

instance A.ToJSON (DnsDomains DNS.Domain) where
    toJSON (DnsDomains addrsList) = toJSON addrsList

instance A.FromJSON (DnsDomains DNS.Domain) where
    parseJSON (A.Array vec) = do
        final <- mapM A.parseJSON (toList vec)
        return $ DnsDomains final
    parseJSON invalid = A.typeMismatch "DnsDomains DNS.Domain" invalid

-- | Node address
data NodeAddr a =
    -- | We specify the exact address of this node
    --
    -- If port unspecified, use the default.
    NodeAddrExact IP (Maybe Word16)

    -- | Do a DNS lookup to find the node's address
    --
    -- If domain unspecified, use the node's name
    -- If port unspecified, use the default.
  | NodeAddrDNS a (Maybe Word16)
  deriving (Eq, Generic, Show)

instance ToJSON (NodeAddr (Maybe DNS.Domain)) where
    toJSON =
        \case
            (NodeAddrExact ip mWord)  ->
                toJSON (NodeAddrExact ip mWord :: NodeAddr DNS.Domain)
            (NodeAddrDNS nHost mWord) ->
                case nHost of
                    Just host -> toJSON $ NodeAddrDNS host mWord
                    Nothing   -> error "Hostname was not specified"

instance FromJSON (NodeAddr (Maybe DNS.Domain)) where
    parseJSON (A.Object o)
        | (HMS.member "addr" o) && (HMS.member "port" o) = do
              addrV <- o .: "addr"
              addr <- parseJSON addrV
              portV <- o .: "port"
              port <- parseJSON portV
              return $ NodeAddrExact addr port
        | (HMS.member "host" o) && (HMS.member "port" o) = do
              portV <- o .: "port"
              port <- parseJSON portV
              hostV <- o .: "host"
              case hostV of
                  A.String text -> do
                      let host = encodeUtf8 @Text @ByteString text
                      return $ NodeAddrDNS (Just host) port
                  _             -> aesonError "Incorrect JSON encoding for hostname"
        | otherwise =
            aesonError "Incorrect JSON encoding for NodeAddr (Maybe DNS.Domain)"
    parseJSON invalid = A.typeMismatch "NodeAddr (Maybe DNS.Domain)" invalid

-- | Orphan instance; lifted from aeson-iproute
instance ToJSON IP where
    toJSON = A.String . T.pack . show

-- | Orphan instance; lifted from aeson-iproute
instance FromJSON IP where
    parseJSON (A.String s)
        | Just r <- readMaybe (T.unpack s) = pure r
        | otherwise = aesonError "Unable to parse IP"
    parseJSON v = A.typeMismatch "IP" v

instance ToJSON (NodeAddr DNS.Domain) where
    toJSON = do
        let defaultPort = 3000 :: Word16
        A.object . \case
            (NodeAddrExact ip mWord) ->
                 case (ip, mWord) of
                     (IPv4 (IP4 ip4), Just port) ->
                         [ "addr" .= (A.String . show $ IPv4 (IP4 ip4))
                         , "port" .= toJSON port
                         ]
                     (IPv4 (IP4 ip4), Nothing)   ->
                         [ "addr" .= (A.String . show $ IPv4 (IP4 ip4))
                         , "port" .= toJSON defaultPort
                         ]
                     (IPv6 (IP6 ip6), Just port) ->
                         [ "addr" .= (A.String . show $ IPv6 (IP6 ip6))
                         , "port" .= toJSON port
                         ]
                     (IPv6 (IP6 ip6), Nothing)   ->
                         [ "addr" .= (A.String . show $ IPv6 (IP6 ip6))
                         , "port" .= toJSON defaultPort
                         ]
            (NodeAddrDNS nHost mWord) ->
                 case (nHost, mWord) of
                     (host, Just port) ->
                         [ "host" .= (toJSON $ decodeUtf8 @Text host)
                         , "port" .= toJSON port
                         ]
                     (host, Nothing)   ->
                         [ "host" .= (toJSON $ decodeUtf8 @Text host)
                         , "port" .= toJSON defaultPort
                         ]

instance FromJSON (NodeAddr DNS.Domain) where
    parseJSON = A.withObject "NodeAddr" $ extractNodeAddr (toAesonError . aux)
      where
        aux :: Maybe DNS.Domain -> Either Text DNS.Domain
        aux Nothing    = Left "Missing domain name or address"
        aux (Just dom) = Right dom

-- | Resolve a list of 'NodeAddr', possibly using DNS.
--
-- Each element may resolve to 0 or more addresses, or fail with some error
-- according to the 'resolve' function.
resolveDnsDomains :: forall a e.
                     (a -> IO (Either e [IPv4])) -- ^ Actual resolution
                  -> Word16                      -- ^ Default port
                  -> [NodeAddr a]
                  -> IO [Either e [(ByteString, Word16)]]
resolveDnsDomains resolve defaultPort dnsDomains' =
    forM dnsDomains' resolveOne
  where

    resolveOne :: NodeAddr a -> IO (Either e [(ByteString, Word16)])
    resolveOne (NodeAddrDNS dom mPort) = do
        let toAddr :: IPv4 -> (ByteString, Word16)
            toAddr ip = (BS.C8.pack (show ip), fromMaybe defaultPort mPort)
        mIPs <- resolve dom
        pure $ case mIPs of
            Left  err -> Left err
            Right ips -> Right $ map toAddr ips
    resolveOne (NodeAddrExact ip mPort) = do
        let port = fromMaybe defaultPort mPort
            ipBS = encodeUtf8 @String . show $ ip
        pure $ Right [(ipBS, port)]

-- Useful when we have a 'NodeAddr' as part of a larger object
extractNodeAddr :: forall a. (Maybe DNS.Domain -> A.Parser a)
                -> A.Object
                -> A.Parser (NodeAddr a)
extractNodeAddr mkA obj = do
    mAddr <- obj .:? "addr"
    mHost <- obj .:? "host"
    mPort <- obj .:? "port"
    case (mAddr, mHost) of
        (Just ipAddr, Nothing) -> do
            -- Make sure `addr` is a proper IP address
            toAesonError $ case readMaybe ipAddr of
                Nothing   -> Left "The value specified in 'addr' is not a \
                                  \valid IP address."
                Just addr -> Right $ NodeAddrExact addr mPort
        (Nothing,  _) -> do
            -- Make sure 'host' is not a valid IP address (which is disallowed)
            case mHost of
                -- User didn't specify a 'host', proceed normally.
                Nothing  -> mkNodeAddrDNS mHost mPort
                Just mbH -> case readMaybe @IP mbH of
                    -- mHost is not an IP, allow it.
                    Nothing -> mkNodeAddrDNS mHost mPort
                    Just _  -> aesonError "The value specified in 'host' is \
                                          \not a valid hostname, but an IP."
        (Just _, Just _)    -> aesonError "Cannot use both 'addr' and 'host'"
  where
    aux :: String -> DNS.Domain
    aux = BS.C8.pack

    mkNodeAddrDNS :: Maybe String -> Maybe Word16 -> A.Parser (NodeAddr a)
    mkNodeAddrDNS mHost mPort = do
        a <- mkA (aux <$> mHost)
        return $ NodeAddrDNS a mPort
