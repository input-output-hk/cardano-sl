{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.Delegation.DB
       ( getPskByIssuer
       , getPskChain
       , getPskForest
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse

       , DelegationOp (..)

       , runDlgTransIterator
       , runDlgTransMapIterator
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
getPskByIssuer
    :: MonadDB m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPskByIssuer (either addressHash identity -> issuer) =
    rocksGetBi (pskKey issuer) =<< getGStateDB

-- | Given an issuer, retrieves all certificate chains starting in
-- issuer. This function performs a series of consequental db reads so
-- it must be used under the shared lock.
getPskChain
    :: MonadDB m
    => Either PublicKey StakeholderId -> m DlgMemPool
getPskChain = getPskChainInternal HS.empty

-- See doc for 'getPskTree'. This function also stops traversal if
-- encounters anyone in 'toIgnore' set.
getPskChainInternal
    :: MonadDB m
    => HashSet StakeholderId -> Either PublicKey StakeholderId -> m DlgMemPool
getPskChainInternal toIgnore (either addressHash identity -> issuer) =
    fmap (view _1) $ flip execStateT (HM.empty, [issuer], HS.empty) trav
  where
    trav = use _2 >>= \case
        []                           -> pass
        (x:_) | HS.member x toIgnore -> (_2 %= drop 1) >> trav
        (x:_)                        -> do
            whenM (uses _3 $ HS.member x) $
                throwM $ DBMalformed "getPskTree: found a PSK loop"
            _2 %= drop 1
            pskM <- lift $ getPskByIssuer $ Right x
            whenJust pskM $ \psk -> do
                let is = pskIssuerPk psk
                _1 %= HM.insert is psk
                _3 %= HS.insert (addressHash is)
            trav

-- | Retrieves certificate forest, where given issuers are trees'
-- leaves. Executes 'getPskChain' for every issuer and merges. This
-- function must be used under outside shared lock.
getPskForest
    :: (MonadDB m)
    => Either [PublicKey] [StakeholderId] -> m DlgMemPool
getPskForest (either (fmap addressHash) identity -> issuers) =
    foldlM foldFoo HM.empty (map Right issuers)
  where
    -- Don't revisit branches we retrieved earlier.
    foldFoo cur = getPskChainInternal (HS.fromList $ map addressHash $ HM.keys cur)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDB m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDlgTransitive :: MonadDB m => PublicKey -> m (Maybe PublicKey)
getDlgTransitive iPk = rocksGetBi (transDlgKey iPk) =<< getGStateDB

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDB m => PublicKey -> m [PublicKey]
getDlgTransitiveReverse dPk =
    fmap (fromMaybe []) . rocksGetBi (transRevDlgKey dPk) =<< getGStateDB

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPsk !ProxySKHeavy
    -- ^ Adds Psk. Overwrites if present.
    | DelPsk !PublicKey
    -- ^ Removes PSK by issuer PK.
    | AddTransitiveDlg PublicKey PublicKey
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg PublicKey
    -- ^ Remove i -> d link for i.
    | SetTransitiveDlgRev PublicKey [PublicKey]
    -- ^ Set value to map d -> [i], reverse index of transitive dlg

instance RocksBatchOp DelegationOp where
    toBatchOp (AddPsk psk)
        | pskIssuerPk psk == pskDelegatePk psk = [] -- panic maybe
        | otherwise =
            [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk)
                       (encodeStrict psk)]
    toBatchOp (DelPsk issuerPk) = [Rocks.Del $ pskKey $ addressHash issuerPk]
    toBatchOp (AddTransitiveDlg iPk dPk) = [Rocks.Put (transDlgKey iPk) (encodeStrict dPk)]
    toBatchOp (DelTransitiveDlg iPk) = [Rocks.Del $ transDlgKey iPk]
    toBatchOp (SetTransitiveDlgRev dPk []) = [Rocks.Del $ transRevDlgKey dPk]
    toBatchOp (SetTransitiveDlgRev dPk iPks) =
        [Rocks.Put (transRevDlgKey dPk) (encodeStrict iPks)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- Transitive relation iteration
data DlgTransIter

instance DBIteratorClass DlgTransIter where
    type IterKey DlgTransIter = PublicKey
    type IterValue DlgTransIter = PublicKey
    iterKeyPrefix _ = iterTransPrefix

runDlgTransIterator
    :: forall m a . MonadDB m
    => DBnIterator DlgTransIter a -> m a
runDlgTransIterator = runDBnIterator @DlgTransIter _gStateDB

runDlgTransMapIterator
    :: forall v m a . MonadDB m
    => DBnMapIterator DlgTransIter v a -> (IterType DlgTransIter -> v) -> m a
runDlgTransMapIterator = runDBnMapIterator @DlgTransIter _gStateDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey s = "d/p/" <> encodeStrict s

transDlgKey :: PublicKey -> ByteString
transDlgKey = encodeWithKeyPrefix @DlgTransIter

iterTransPrefix :: ByteString
iterTransPrefix = "d/t/"

-- Reverse index of iterTransitive
transRevDlgKey :: PublicKey -> ByteString
transRevDlgKey pk = "d/tb/" <> encodeStrict pk
