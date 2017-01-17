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
       ) where

import           Data.Maybe       (isJust)
import qualified Database.RocksDB as Rocks
import           Universum

import           Pos.Binary.Class (encodeStrict)
import           Pos.Crypto       (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class     (MonadDB, getUtxoDB)
import           Pos.DB.Functions (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.Iterator  (DBMapIterator, IterType, MonadDBIterator (..),
                                   mapIterator)
import           Pos.Types        (ProxySKSimple, StakeholderId, addressHash)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer address (hash of public key) if present.
getPSKByIssuerAddressHash :: MonadDB ssc m => StakeholderId -> m (Maybe ProxySKSimple)
getPSKByIssuerAddressHash addrHash = rocksGetBi (pskKey addrHash) =<< getUtxoDB

-- | Retrieves certificate by issuer public key if present.
getPSKByIssuer :: MonadDB ssc m => PublicKey -> m (Maybe ProxySKSimple)
getPSKByIssuer = getPSKByIssuerAddressHash . addressHash

isIssuerByAddressHash :: MonadDB ssc m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPSKByIssuerAddressHash

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPSK !ProxySKSimple
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

instance MonadDBIterator PskIter where
    type IterKey PskIter = StakeholderId
    type IterValue PskIter = ProxySKSimple
    iterKeyPrefix _ = "d/"

runPskIterator
    :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
    => DBMapIterator PskIter v m a -> (IterType PskIter -> v) -> m a
runPskIterator iter f = mapIterator @PskIter @v iter f =<< getUtxoDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKSimple
pskKey :: StakeholderId -> ByteString
pskKey = encodeWithKeyPrefix @PskIter
