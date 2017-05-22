{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.Delegation.DB
       ( getPSKByIssuer
       , getPSKTree
       , getPSKTreeAll
       , isIssuerByAddressHash

       , DelegationOp (..)

       , runPskIterator
       , runPskMapIterator
       ) where

import           Control.Lens        (uses, (%=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Binary.Class    (encodeStrict)
import           Pos.Crypto          (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Error        (DBError (DBMalformed))
import           Pos.DB.Functions    (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.Iterator     (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                      IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types        (NodeDBs (_gStateDB))
import           Pos.Types           (ProxySKHeavy, StakeholderId, addressHash)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer public key or his
-- address/stakeholder id, if present.
getPSKByIssuer
    :: MonadDB m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPSKByIssuer (either addressHash identity -> issuer) =
    rocksGetBi (pskKey issuer) =<< getUtxoDB

-- | Given an issuer, retrieves all certificates
getPSKTree
    :: MonadDB m
    => Either PublicKey StakeholderId -> m (HashMap PublicKey ProxySKHeavy)
getPSKTree (either addressHash identity -> issuer) =
    fmap (view _1) $ flip execStateT (HM.empty, [issuer], HS.empty) bfs
  where
    bfs = use _2 >>= \case
        [] -> pass
        (x:_) -> do
            whenM (uses _3 $ HS.member x) $
                throwM $ DBMalformed "getPSKTree: found a PSK loop"
            _2 %= drop 1
            pskM <- lift $ getPSKByIssuer $ Right x
            whenJust pskM $ \psk -> do
                let is = pskIssuerPk psk
                _1 %= HM.insert is psk
                _3 %= HS.insert (addressHash is)
            bfs

-- | Retrieves hashmap from all issuers supplied. See 'getPSKTree'.
getPSKTreeAll
    :: (MonadDB m)
    => Either [PublicKey] [StakeholderId]
    -> m (HashMap PublicKey ProxySKHeavy)
getPSKTreeAll (either (fmap addressHash) identity -> issuers) =
    mconcat <$> mapM getPSKTree (map Right issuers)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDB m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPSKByIssuer . Right

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
