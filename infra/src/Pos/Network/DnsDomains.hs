module Pos.Network.DnsDomains
       ( DnsDomains(..)
       , NodeAddr(..)
       , resolveDnsDomains
       ) where


import           Universum

import qualified Data.ByteString.Char8 as BS.C8
import           Data.IP (IP, IPv4)
import           Network.Broadcast.OutboundQueue.Types (AllOf, Alts)

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
  deriving (Show)

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
  deriving (Show)

-- | Resolve a list of 'NodeAddr', possibly using DNS.
--
-- Each element may resolve to 0 or more addresses, or fail with some error
-- according to the 'resolve' function.
resolveDnsDomains :: forall a e.
                     (a -> IO (Either e [IPv4])) -- ^ Actual resolution
                  -> Word16                      -- ^ Default port
                  -> [NodeAddr a]
                  -> IO [Either e [(ByteString, Word16)]]
resolveDnsDomains resolve defaultPort dnsDomains =
    forM dnsDomains resolveOne
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
