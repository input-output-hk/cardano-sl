-- | Interface for the Misc DB

module Pos.Modern.DB.Misc
       (
         getProxySecretKeys
       , addProxySecretKey
       , putSecretKeyHash
       , checkSecretKeyHash
       ) where

import           Universum

import           Pos.Binary              (Bi)
import           Pos.Crypto              (Hash, ProxySecretKey, SecretKey)
import           Pos.Modern.DB.Class     (MonadDB, getMiscDB)
import           Pos.Modern.DB.Functions (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.Types               (EpochIndex)

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

type PSK = ProxySecretKey (EpochIndex, EpochIndex)

-- | Gets proxy secret keys stored by node
getProxySecretKeys :: MonadDB ssc m => m [PSK]
getProxySecretKeys = do
    curCerts <- getBi @([PSK]) certsKey
    maybe onNothing pure curCerts
  where
    onNothing = do
        putBi certsKey ([] :: [PSK])
        pure []

-- | Adds proxy secret key if not present. Nothing if present.
addProxySecretKey :: MonadDB ssc m => PSK -> m ()
addProxySecretKey psk = do
    keys <- getProxySecretKeys
    putBi certsKey (psk:keys)

-- | Puts or overwrites secret key of the node. Returns if it was
-- overwritten.
putSecretKeyHash :: MonadDB ssc m => Hash SecretKey -> m Bool
putSecretKeyHash h = do
    curSkHash <- getBi @(Hash SecretKey) certsKey
    putBi skHashKey h
    pure $ isJust curSkHash

-- | Checks if given secret key hash matches the hash in the
-- database. Puts it into the database and return True if nothing was
-- stored there.
checkSecretKeyHash :: MonadDB ssc m => Hash SecretKey -> m Bool
checkSecretKeyHash h = do
    curSkHash <- getBi @(Hash SecretKey) certsKey
    maybe (putBi skHashKey h >> pure True) (pure . (== h)) curSkHash

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: forall v ssc m . (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getMiscDB

putBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getMiscDB

certsKey :: ByteString
certsKey = "certs"

skHashKey :: ByteString
skHashKey = "skhash"
