{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.Delegation.DB
       ( getPSKByIssuerAddressHash
       , getPSKByIssuer
       , isIssuerByAddressHash
       , DelegationOp (..)

       , runPskIterator
       , runPskMapIterator

       , getDelegators
       ) where

import           Universum

import qualified Data.HashMap.Strict  as HM
import qualified Database.RocksDB     as Rocks

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Crypto           (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class         (MonadDB, MonadDBPure)
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.DB.Iterator      (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                       IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types         (NodeDBs (_gStateDB))
import           Pos.Types            (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Util.Iterator    (nextItem)

----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer address (hash of public key) if present.
getPSKByIssuerAddressHash :: MonadDBPure m => StakeholderId -> m (Maybe ProxySKHeavy)
getPSKByIssuerAddressHash addrHash = gsGetBi (pskKey addrHash)

-- | Retrieves certificate by issuer public key if present.
getPSKByIssuer :: MonadDBPure m => PublicKey -> m (Maybe ProxySKHeavy)
getPSKByIssuer = getPSKByIssuerAddressHash . addressHash

isIssuerByAddressHash :: (MonadDBPure m) => StakeholderId -> m Bool
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

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | For each stakeholder, say who has delegated to that stakeholder.
--
-- NB. It's not called @getIssuers@ because we already have issuers (i.e.
-- block issuers)
getDelegators :: MonadDB m => m (HashMap StakeholderId [StakeholderId])
getDelegators = runPskMapIterator (step mempty) conv
  where
    step hm = nextItem >>= maybe (pure hm) (\(iss, del) -> do
        let curList = HM.lookupDefault [] del hm
        step (HM.insert del (iss:curList) hm))
    conv (id, cert) = (id, addressHash (pskDelegatePk cert))
