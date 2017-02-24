{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.DB.GState.Delegation
       ( getPSKByIssuerAddressHash
       , getPSKByIssuer
       , isIssuerByAddressHash
       , DelegationOp (..)

       , runPskIterator
       , runPskMapIterator
       ) where

import           Data.Maybe       (isJust)
import qualified Database.RocksDB as Rocks
import           Universum

import           Pos.Binary.Class (encodeStrict)
import           Pos.Crypto       (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class     (MonadDB, getUtxoDB)
import           Pos.DB.Functions (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.Iterator  (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                   IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types     (NodeDBs (_gStateDB))
import           Pos.Types        (ProxySKHeavy, StakeholderId, addressHash)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer address (hash of public key) if present.
getPSKByIssuerAddressHash :: MonadDB m => StakeholderId -> m (Maybe ProxySKHeavy)
getPSKByIssuerAddressHash addrHash = rocksGetBi (pskKey addrHash) =<< getUtxoDB

-- | Retrieves certificate by issuer public key if present.
getPSKByIssuer :: MonadDB m => PublicKey -> m (Maybe ProxySKHeavy)
getPSKByIssuer = getPSKByIssuerAddressHash . addressHash

isIssuerByAddressHash :: MonadDB m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPSKByIssuerAddressHash

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPSK !ProxySKHeavy
    -- ^ Adds PSK. Overwrites if present.
    | DelPSK !PublicKey
    -- ^ Removes PSK by issuer PK.

instance RocksBatchOp DelegationOp where
    toBatchOp (AddPSK psk)
        | pskIssuerPk psk == pskDelegatePk psk = [] -- panic maybe
        | otherwise =
            [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk)
                       (encodeStrict psk)]
    toBatchOp (DelPSK issuerPk) =
        [Rocks.Del $ pskKey $ addressHash issuerPk]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PskIter

instance DBIteratorClass PskIter where
    type IterKey PskIter = StakeholderId
    type IterValue PskIter = ProxySKHeavy
    iterKeyPrefix _ = iterationPrefix

runPskIterator
    :: forall m a . MonadDB m
    => DBnIterator PskIter a -> m a
runPskIterator = runDBnIterator @PskIter _gStateDB

runPskMapIterator
    :: forall v m a . MonadDB m
    => DBnMapIterator PskIter v a -> (IterType PskIter -> v) -> m a
runPskMapIterator = runDBnMapIterator @PskIter _gStateDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey = encodeWithKeyPrefix @PskIter

iterationPrefix :: ByteString
iterationPrefix = "d/p/"
