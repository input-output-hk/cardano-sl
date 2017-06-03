{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface for the Misc DB

module Pos.DB.Misc
       (
         prepareMiscDB

       , getProxySecretKeys
       , addProxySecretKey
       , removeProxySecretKey
       , dropOldProxySecretKeys

       , putSecretKeyHash
       , checkSecretKeyHash
       ) where

import           Universum

import           Pos.Binary.Ssc     ()
import           Pos.Crypto         (Hash, PublicKey, SecretKey, pskIssuerPk, pskOmega)
import           Pos.DB.Class       (MonadRealDB)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.Types          (EpochIndex, ProxySKLight)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareMiscDB
    :: MonadRealDB m
    => m ()
prepareMiscDB = pass

----------------------------------------------------------------------------
-- Delegation and proxy signing
----------------------------------------------------------------------------

-- | Gets proxy secret keys stored by node. We store only "related"
-- psks: those who have pskIssuer or pskDelegate matching our pk.
getProxySecretKeys :: MonadRealDB m => m [ProxySKLight]
getProxySecretKeys = do
    curCerts <- miscGetBi @([ProxySKLight]) proxySKKey
    maybe onNothing pure curCerts
  where
    onNothing = do
        miscPutBi proxySKKey ([] :: [ProxySKLight])
        pure []

-- | Adds proxy secret key if not present. Nothing if present. Call
-- this under write lock only (see 'miscLock').
addProxySecretKey :: MonadRealDB m => ProxySKLight -> m ()
addProxySecretKey psk = do
    keys <- getProxySecretKeys
    miscPutBi proxySKKey $ ordNub $ psk:keys

-- | Removes proxy secret key if present by issuer pk. Call it under
-- write lock only (see 'miscLock').
removeProxySecretKey :: MonadRealDB m => PublicKey -> m ()
removeProxySecretKey pk = do
    keys <- getProxySecretKeys
    miscPutBi proxySKKey $ filter ((/= pk) . pskIssuerPk) keys

-- TODO it's not used anywhere yet. Should it be?
-- | Given epochindex, throws away all outdated PSKs. Remark: it
-- doesn't remove keys that can be used in future. Call it under write
-- lock only (see 'miscLock').
dropOldProxySecretKeys :: MonadRealDB m => EpochIndex -> m ()
dropOldProxySecretKeys eId = do
    keys <- filter (\p -> eId <= snd (pskOmega p)) <$>
            getProxySecretKeys
    miscPutBi proxySKKey keys

----------------------------------------------------------------------------
-- Secret key storage & verification
--
-- Currently node is allowed to have only one secret key, so its hash
-- is stored in the storage and can be checked/overwritten (e.g. in order
-- to exit if tried to launch with another key).
----------------------------------------------------------------------------

-- | Puts or overwrites secret key of the node. Returns if it was
-- overwritten.
putSecretKeyHash :: MonadRealDB m => Hash SecretKey -> m Bool
putSecretKeyHash h = do
    curSkHash <- miscGetBi @(Hash SecretKey) skHashKey
    miscPutBi skHashKey h
    pure $ isJust curSkHash

-- | Checks if given secret key hash matches the hash in the
-- database. Puts it into the database and return True if nothing was
-- stored there.
checkSecretKeyHash :: MonadRealDB m => Hash SecretKey -> m Bool
checkSecretKeyHash h = do
    curSkHash <- miscGetBi @(Hash SecretKey) skHashKey
    maybe (miscPutBi skHashKey h >> pure True) (pure . (== h)) curSkHash

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

proxySKKey :: ByteString
proxySKKey = "psk/"

skHashKey :: ByteString
skHashKey = "skh/"
