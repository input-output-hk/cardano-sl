{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface for the Misc DB

module Pos.DB.Misc
       (
         prepareMiscDB

       , getProxySecretKeys
       , addProxySecretKey
       , dropOldProxySecretKeys

       , getSecretStorage
       , putSecretStorage

       , putSecretKeyHash
       , checkSecretKeyHash
       ) where

import           Data.Default                    (def)
import           Universum

import           Pos.Binary.Class                (Bi)
import           Pos.Binary.Ssc                  ()
import           Pos.Crypto                      (Hash, SecretKey, pskOmega)
import           Pos.DB.Class                    (MonadDB, getMiscDB)
import           Pos.DB.Functions                (rocksGetBi, rocksPutBi)
import           Pos.Ssc.GodTossing.Secret.Types (GtSecretStorage)
import           Pos.Ssc.GodTossing.Types.Type   (SscGodTossing)
import           Pos.Types                       (EpochIndex, ProxySKEpoch)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareMiscDB
    :: forall ssc m.
       (MonadDB ssc m)
    => m ()
prepareMiscDB = pass

----------------------------------------------------------------------------
-- Delegation and proxy signing
----------------------------------------------------------------------------

-- | Gets proxy secret keys stored by node
getProxySecretKeys :: MonadDB ssc m => m [ProxySKEpoch]
getProxySecretKeys = do
    curCerts <- getBi @([ProxySKEpoch]) proxySKKey
    maybe onNothing pure curCerts
  where
    onNothing = do
        putBi proxySKKey ([] :: [ProxySKEpoch])
        pure []

-- | Adds proxy secret key if not present. Nothing if present.
addProxySecretKey :: MonadDB ssc m => ProxySKEpoch -> m ()
addProxySecretKey psk = do
    keys <- getProxySecretKeys
    putBi proxySKKey (psk:keys)

-- | Given epochindex, throws away all outdated PSKs. Remark: it
-- doesn't remove keys that can be used in future.
dropOldProxySecretKeys :: MonadDB ssc m => EpochIndex -> m ()
dropOldProxySecretKeys eId = do
    keys <- filter (\p -> eId <= snd (pskOmega p)) <$>
            getProxySecretKeys
    putBi proxySKKey keys

----------------------------------------------------------------------------
-- Secret key storage & verification
--
-- Currently node is allowed to have only one secret key, so its hash
-- is stored in the storage and can be checked/overwritten (e.g. in order
-- to exit if tried to launch with another key).
----------------------------------------------------------------------------

-- | Puts or overwrites secret key of the node. Returns if it was
-- overwritten.
putSecretKeyHash :: MonadDB ssc m => Hash SecretKey -> m Bool
putSecretKeyHash h = do
    curSkHash <- getBi @(Hash SecretKey) skHashKey
    putBi skHashKey h
    pure $ isJust curSkHash

-- | Checks if given secret key hash matches the hash in the
-- database. Puts it into the database and return True if nothing was
-- stored there.
checkSecretKeyHash :: MonadDB ssc m => Hash SecretKey -> m Bool
checkSecretKeyHash h = do
    curSkHash <- getBi @(Hash SecretKey) skHashKey
    maybe (putBi skHashKey h >> pure True) (pure . (== h)) curSkHash

----------------------------------------------------------------------------
-- Ssc Secret Storage
----------------------------------------------------------------------------
getSecretStorage :: MonadDB SscGodTossing m => m GtSecretStorage
getSecretStorage =
    getBi @GtSecretStorage secretStorageKey >>=
    maybe createSecretStorage pure
  where
    createSecretStorage =
        def <$ putBi @GtSecretStorage secretStorageKey def

putSecretStorage :: MonadDB SscGodTossing m => GtSecretStorage -> m ()
putSecretStorage = putBi @GtSecretStorage secretStorageKey

secretStorageKey :: ByteString
secretStorageKey = "gtSecretStorageKey"

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: forall v ssc m . (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getMiscDB

putBi
    :: forall v ssc m . (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getMiscDB

proxySKKey :: ByteString
proxySKKey = "psk_"

skHashKey :: ByteString
skHashKey = "skhash_"
