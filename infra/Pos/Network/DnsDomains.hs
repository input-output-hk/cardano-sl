
module Pos.Network.DnsDomains (
    DnsDomains(..)
  , NodeAddr(..)
  , resolveDnsDomains
  ) where

import qualified Data.ByteString.Char8                 as BS.C8
import           Data.IP                               (IP, IPv4)
import           Network.Broadcast.OutboundQueue.Types (AllOf, Alts)
import           Universum

-- | DNS domains for relay discovery
--
-- We provide a list of list of domain names to query. The outer list
-- corresponds to backup DNS servers; the inner lists provide multiple
-- domain names which serve different purposes (e.g., the first might be
-- configured to return geolocated hosts, with the second a load-balancing
-- fall-back). The idea is that querying each of the elements of the inner
-- lists provides a complete set of relay nodes to be tried, using the
-- backup domain names only when one or more of the primary ones fail.
data DnsDomains a = DnsDomains {
      dnsDomains :: !(Alts (AllOf (NodeAddr a)))
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

-- | Resolve 'DnsDomains'
--
-- See 'DnsDomains' for an explanation of the logic. This only returns a set
-- of errors if _all_ alternatives fail; in that case, it returns the (first)
-- error that occurred for all alternatives (we don't continue trying a set of
-- domains after the first error).
resolveDnsDomains :: forall a e.
                     (a -> IO (Either e [IPv4])) -- ^ Actual resolution
                  -> Word16                      -- ^ Default port
                  -> DnsDomains a
                  -> IO (Either [e] [(ByteString, Word16)])
resolveDnsDomains resolve defaultPort (DnsDomains{..}) =
    findAlts [] dnsDomains
  where
    -- Find a set of DNS names all of which we can resolve together
    findAlts :: [e]
             -> Alts (AllOf (NodeAddr a))
             -> IO (Either [e] [(ByteString, Word16)])
    findAlts errs []           = return $ Left (reverse errs)
    findAlts errs (alts:altss) = do
      mAddrs <- tryAlts alts
      case mAddrs of
        Left  err   -> findAlts (err:errs) altss
        Right addrs -> return $ Right addrs

    -- Resolve a set of domains names. If they can all be resolved, return
    -- the set; if an error occurs, return Nothing.
    tryAlts :: AllOf (NodeAddr a) -> IO (Either e [(ByteString, Word16)])
    tryAlts = go []
      where
        go :: [(ByteString, Word16)] -> [NodeAddr a] -> IO (Either e [(ByteString, Word16)])
        go acc []         = return $ Right (reverse acc)
        go acc (NodeAddrDNS dom mPort:addrs) = do
          let toAddr :: IPv4 -> (ByteString, Word16)
              toAddr ip = (BS.C8.pack (show ip), fromMaybe defaultPort mPort)
          mIPs <- resolve dom
          case mIPs of
            Left  err -> return $ Left err
            Right ips -> go (reverse (map toAddr ips) ++ acc) addrs
        go acc (NodeAddrExact ip mPort:addrs) = do
          let port = fromMaybe defaultPort mPort
              ipBS = encodeUtf8 @String . show $ ip
          go ((ipBS, port):acc) addrs
