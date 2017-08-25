-- | Interface for the Misc DB

module Pos.DB.Misc
       (
         prepareMiscDB

       , getProxySecretKeysLight
       , addProxySecretKey
       , removeProxySecretKey
       , dropOldProxySecretKeys
       ) where

import           Universum

import           Pos.Binary.Ssc     ()
import           Pos.Crypto         (PublicKey, pskIssuerPk, pskOmega)
import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.Types          (EpochIndex, ProxySKLight)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareMiscDB
    :: MonadDB m
    => m ()
prepareMiscDB = pass

----------------------------------------------------------------------------
-- Delegation and proxy signing
----------------------------------------------------------------------------

-- | Gets proxy secret keys stored by node. We store only "related"
-- psks: those who have pskIssuer or pskDelegate matching our pk.
getProxySecretKeysLight :: MonadDB m => m [ProxySKLight]
getProxySecretKeysLight = do
    curCerts <- miscGetBi @([ProxySKLight]) proxySKKey
    maybe onNothing pure curCerts
  where
    onNothing = do
        miscPutBi proxySKKey ([] :: [ProxySKLight])
        pure []

-- | Adds proxy secret key if not present. Nothing if present. Call
-- this under write lock only (see 'miscLock').
addProxySecretKey :: MonadDB m => ProxySKLight -> m ()
addProxySecretKey psk = do
    keys <- getProxySecretKeysLight
    miscPutBi proxySKKey $ ordNub $ psk:keys

-- | Removes proxy secret key if present by issuer pk. Call it under
-- write lock only (see 'miscLock').
removeProxySecretKey :: MonadDB m => PublicKey -> m ()
removeProxySecretKey pk = do
    keys <- getProxySecretKeysLight
    miscPutBi proxySKKey $ filter ((/= pk) . pskIssuerPk) keys

-- TODO it's not used anywhere yet. Should it be?
-- | Given epochindex, throws away all outdated PSKs. Remark: it
-- doesn't remove keys that can be used in future. Call it under write
-- lock only (see 'miscLock').
dropOldProxySecretKeys :: MonadDB m => EpochIndex -> m ()
dropOldProxySecretKeys eId = do
    keys <- filter (\p -> eId <= snd (pskOmega p)) <$>
            getProxySecretKeysLight
    miscPutBi proxySKKey keys

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

proxySKKey :: ByteString
proxySKKey = "psk/"
