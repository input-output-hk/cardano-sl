{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.Delegation.DB
       ( getPSKByIssuer
       , getPSKChain
       , getPSKForest
       , isIssuerByAddressHash
       , getDelegationTransitive

       , DelegationOp (..)

       , runPskTransIterator
       , runPskTransMapIterator
       ) where

import           Control.Lens         (uses, (%=))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Crypto           (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class         (MonadDB, getGStateDB)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.Iterator      (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                       IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types         (NodeDBs (_gStateDB))
import           Pos.Delegation.Types (DlgMemPool)
import           Pos.Types            (ProxySKHeavy, StakeholderId, addressHash)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer public key or his
-- address/stakeholder id, if present.
getPSKByIssuer
    :: MonadDB m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPSKByIssuer (either addressHash identity -> issuer) =
    rocksGetBi (pskKey issuer) =<< getGStateDB

-- | Given an issuer, retrieves all certificate chains starting in
-- issuer. This function performs a series of consequental db reads so
-- it must be used under the shared lock.
getPSKChain
    :: MonadDB m
    => Either PublicKey StakeholderId -> m DlgMemPool
getPSKChain = getPSKChainInternal HS.empty

-- See doc for 'getPSKTree'. This function also stops traversal if
-- encounters anyone in 'toIgnore' set.
getPSKChainInternal
    :: MonadDB m
    => HashSet StakeholderId -> Either PublicKey StakeholderId -> m DlgMemPool
getPSKChainInternal toIgnore (either addressHash identity -> issuer) =
    fmap (view _1) $ flip execStateT (HM.empty, [issuer], HS.empty) trav
  where
    trav = use _2 >>= \case
        []                           -> pass
        (x:_) | HS.member x toIgnore -> (_2 %= drop 1) >> trav
        (x:_)                        -> do
            whenM (uses _3 $ HS.member x) $
                throwM $ DBMalformed "getPSKTree: found a PSK loop"
            _2 %= drop 1
            pskM <- lift $ getPSKByIssuer $ Right x
            whenJust pskM $ \psk -> do
                let is = pskIssuerPk psk
                _1 %= HM.insert is psk
                _3 %= HS.insert (addressHash is)
            trav

-- | Retrieves certificate forest, where given issuers are trees'
-- leaves. Executes 'getPSKChain' for every issuer and merges. This
-- function must be used under outside shared lock.
getPSKForest
    :: (MonadDB m)
    => Either [PublicKey] [StakeholderId] -> m DlgMemPool
getPSKForest (either (fmap addressHash) identity -> issuers) =
    foldlM foldFoo HM.empty (map Right issuers)
  where
    -- Don't revisit branches we retrieved earlier.
    foldFoo cur = getPSKChainInternal (HS.fromList $ map addressHash $ HM.keys cur)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDB m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPSKByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDelegationTransitive :: MonadDB m => PublicKey -> m (Maybe PublicKey)
getDelegationTransitive iPk = rocksGetBi (transPskKey iPk) =<< getGStateDB

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPSK !ProxySKHeavy
    -- ^ Adds PSK. Overwrites if present.
    | DelPSK !PublicKey
    -- ^ Removes PSK by issuer PK.
    | AddTransitiveDlg PublicKey PublicKey
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg PublicKey
    -- ^ Remove i -> d link for i.

instance RocksBatchOp DelegationOp where
    toBatchOp (AddPSK psk)
        | pskIssuerPk psk == pskDelegatePk psk = [] -- panic maybe
        | otherwise =
            [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk)
                       (encodeStrict psk)]
    toBatchOp (DelPSK issuerPk) = [Rocks.Del $ pskKey $ addressHash issuerPk]
    toBatchOp (AddTransitiveDlg iPk dPk) = [Rocks.Put (transPskKey iPk) (encodeStrict dPk)]
    toBatchOp (DelTransitiveDlg iPk) = [Rocks.Del $ transPskKey iPk]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- Transitive relation iteration
data PskTransIter

instance DBIteratorClass PskTransIter where
    type IterKey PskTransIter = PublicKey
    type IterValue PskTransIter = PublicKey
    iterKeyPrefix _ = iterTransPrefix

runPskTransIterator
    :: forall m a . MonadDB m
    => DBnIterator PskTransIter a -> m a
runPskTransIterator = runDBnIterator @PskTransIter _gStateDB

runPskTransMapIterator
    :: forall v m a . MonadDB m
    => DBnMapIterator PskTransIter v a -> (IterType PskTransIter -> v) -> m a
runPskTransMapIterator = runDBnMapIterator @PskTransIter _gStateDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey s = "d/p/" <> encodeStrict s

transPskKey :: PublicKey -> ByteString
transPskKey = encodeWithKeyPrefix @PskTransIter

iterTransPrefix :: ByteString
iterTransPrefix = "d/t/"
