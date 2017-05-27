{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight
-- delegation.
--
-- It stores three mappings:
--
-- 1. Psk mapping: Issuer → PSK, where pskIssuer of PSK is Issuer (must
-- be consistent). We don't store revocation psks, instead we just
-- delete previous Issuer → PSK. DB must not contain revocation psks.
--
-- 2. Dlg transitive mapping: Issuer → Delegate. This one is
-- transitive relation "i delegated to d through some chain of
-- certificates". DB must not contain cycles in psk mapping. As
-- mappings of kind I → I are forbidden (no revocation psks), Delegate
-- is always different from Issuer.
--
-- 3. Dlg reverse transitive mapping: Delegate →
-- Issuers@{Issuer_i}. Corresponds to Issuer → Delegate ∈ Dlg transitive
-- mapping. Notice: here also Delegate ∉ Issuers (see (2)).

module Pos.Delegation.DB
       ( getPskByIssuer
       , getPskChain
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse

       , DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer
       , withEActionsResolve

       , DelegationOp (..)

       , runDlgTransIterator
       , runDlgTransMapIterator

       , getDelegators
       ) where

import           Universum

import           Control.Lens           (uses, (%=))
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Database.RocksDB       as Rocks

import           Pos.Binary.Class       (encodeStrict)
import           Pos.Crypto             (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB                 (DBError (DBMalformed))
import           Pos.DB.Class           (MonadDB, MonadDBPure)
import           Pos.DB.Functions       (RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.GState.Common   (gsGetBi)
import           Pos.DB.Iterator        (DBIteratorClass (..), DBnIterator,
                                         DBnMapIterator, IterType, runDBnIterator,
                                         runDBnMapIterator)
import           Pos.DB.Types           (NodeDBs (_gStateDB))
import           Pos.Delegation.Helpers (isRevokePsk)
import           Pos.Delegation.Types   (DlgMemPool)
import           Pos.Types              (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Util.Iterator      (nextItem)

----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer public key or his
-- address/stakeholder id, if present.
getPskByIssuer
    :: MonadDBPure m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPskByIssuer (either addressHash identity -> issuer) = gsGetBi (pskKey issuer)

-- | Given an issuer, retrieves all certificate chains starting in
-- issuer. This function performs a series of consequental db reads so
-- it must be used under the shared lock.
getPskChain
    :: MonadDBPure m
    => Either PublicKey StakeholderId -> m DlgMemPool
getPskChain = getPskChainInternal HS.empty

-- See doc for 'getPskTree'. This function also stops traversal if
-- encounters anyone in 'toIgnore' set. This may be used to call it
-- several times to collect a whole tree/forest, for example.
getPskChainInternal
    :: MonadDBPure m
    => HashSet StakeholderId -> Either PublicKey StakeholderId -> m DlgMemPool
getPskChainInternal toIgnore (either addressHash identity -> issuer) =
    -- State is tuple of returning mempool and "used flags" set.
    view _1 <$> execStateT (trav issuer) (HM.empty, HS.empty)
  where
    trav x | HS.member x toIgnore = pass
    trav x = do
        whenM (uses _2 $ HS.member x) $
            throwM $ DBMalformed "getPskChainInternal: found a PSK cycle"
        _2 %= HS.insert x
        pskM <- lift $ getPskByIssuer $ Right x
        whenJust pskM $ \psk -> do
            when (isRevokePsk psk) $ throwM $ DBMalformed $
                "getPskChainInternal: found redeem psk: " <> pretty psk
            _1 %= HM.insert (pskIssuerPk psk) psk
            trav (addressHash $ pskDelegatePk psk)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDBPure m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDlgTransitive :: MonadDBPure m => PublicKey -> m (Maybe PublicKey)
getDlgTransitive iPk = gsGetBi (transDlgKey iPk)

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBPure m => PublicKey -> m (HashSet PublicKey)
getDlgTransitiveReverse dPk = fromMaybe mempty <$> gsGetBi (transRevDlgKey dPk)

----------------------------------------------------------------------------
-- DlgEdgeAction and friends
----------------------------------------------------------------------------

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !PublicKey
    deriving (Show, Eq, Generic)

instance Hashable DlgEdgeAction

-- | Converts mempool to set of database actions.
pskToDlgEdgeAction :: ProxySKHeavy -> DlgEdgeAction
pskToDlgEdgeAction psk
    | isRevokePsk psk = DlgEdgeDel (pskIssuerPk psk)
    | otherwise = DlgEdgeAdd psk

-- | Gets issuer of edge action (u from the edge uv).
dlgEdgeActionIssuer :: DlgEdgeAction -> PublicKey
dlgEdgeActionIssuer = \case
    (DlgEdgeDel iPk) -> iPk
    (DlgEdgeAdd psk) -> pskIssuerPk psk

-- | Resolves issuer to heavy PSK using mapping (1) with given set of
-- changes eActions. This set of changes is a map @i -> eAction@,
-- where @i = dlgEdgeActionIssuer eAction@.
withEActionsResolve
    :: (MonadDBPure m)
    => HashMap PublicKey DlgEdgeAction -> PublicKey -> m (Maybe ProxySKHeavy)
withEActionsResolve eActions iPk =
    case HM.lookup iPk eActions of
        Nothing                -> getPskByIssuer $ Left iPk
        Just (DlgEdgeDel _)    -> pure Nothing
        Just (DlgEdgeAdd psk ) -> pure (Just psk)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = PskFromEdgeAction !DlgEdgeAction
    -- ^ Adds or removes Psk. Overwrites on addition if present.
    | AddTransitiveDlg !PublicKey !PublicKey
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg !PublicKey
    -- ^ Remove i -> d link for i.
    | SetTransitiveDlgRev !PublicKey !(HashSet PublicKey)
    -- ^ Set value to map d -> [i], reverse index of transitive dlg

instance RocksBatchOp DelegationOp where
    toBatchOp (PskFromEdgeAction (DlgEdgeAdd psk))
        | isRevokePsk psk =
          error $ "RocksBatchOp instance: malformed revoke psk in DlgEdgeAdd: " <> pretty psk
        | otherwise =
          [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk) (encodeStrict psk)]
    toBatchOp (PskFromEdgeAction (DlgEdgeDel issuerPk)) =
        [Rocks.Del $ pskKey $ addressHash issuerPk]
    toBatchOp (AddTransitiveDlg iPk dPk) = [Rocks.Put (transDlgKey iPk) (encodeStrict dPk)]
    toBatchOp (DelTransitiveDlg iPk) = [Rocks.Del $ transDlgKey iPk]
    toBatchOp (SetTransitiveDlgRev dPk iPks)
        | HS.null iPks = [Rocks.Del $ transRevDlgKey dPk]
        | otherwise    = [Rocks.Put (transRevDlgKey dPk) (encodeStrict iPks)]

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

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | For each stakeholder, say who has delegated to that stakeholder.
--
-- NB. It's not called @getIssuers@ because we already have issuers (i.e.
-- block issuers)
getDelegators :: MonadDB m => m (HashMap StakeholderId [StakeholderId])
getDelegators = runDlgTransMapIterator (step mempty) identity
  where
    step hm = nextItem >>= maybe (pure hm) (\(addressHash -> iss, addressHash -> del) -> do
        let curList = HM.lookupDefault [] del hm
        step (HM.insert del (iss:curList) hm))
