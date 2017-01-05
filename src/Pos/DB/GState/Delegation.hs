-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.DB.GState.Delegation
       ( getPSK
       , putPSK
       , delPSK
       , DelegationOp (..)
       ) where

import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Crypto           (PublicKey, pskIssuerPk)
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Functions     (RocksBatchOp (..), rocksGetBi)
import           Pos.DB.GState.Common (delete, putBi)
import           Pos.Types            (ProxySKSimple)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

getPSK :: MonadDB ssc m => PublicKey -> m (Maybe ProxySKSimple)
getPSK pk = rocksGetBi (pskKey pk) =<< getUtxoDB

putPSK :: MonadDB ssc m => ProxySKSimple -> m ()
putPSK psk = putBi (pskKey $ pskIssuerPk psk) psk

delPSK :: MonadDB ssc m => PublicKey -> m ()
delPSK = delete . pskKey

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPSK ProxySKSimple -- ^ Adds PSK. Overwrites if present.
    | DelPSK PublicKey -- ^ Removes PSK by issuer PK.

instance RocksBatchOp DelegationOp where
    toBatchOp (AddPSK psk) =
        Rocks.Put (pskKey $ pskIssuerPk psk) (encodeStrict psk)
    toBatchOp (DelPSK pk) = Rocks.Del $ pskKey pk

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

pskKey :: PublicKey -> ByteString
pskKey pk = "d/" <> encodeStrict pk
