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
       , getDlgTransPsk
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
import           Formatting             (build, sformat, (%))

import           Pos.Binary.Class       (encodeStrict)
import           Pos.Crypto             (PublicKey, pskDelegatePk, pskIssuerPk,
                                         verifyProxySecretKey)
import           Pos.DB                 (DBError (DBMalformed), RocksBatchOp (..),
                                         encodeWithKeyPrefix)
import           Pos.DB.Class           (MonadDBRead, MonadRealDB)
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
    :: MonadDBRead m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPskByIssuer (either addressHash identity -> issuer) = gsGetBi (pskKey issuer)

-- | Given an issuer, retrieves all certificate chains starting in
-- issuer. This function performs a series of consequental db reads so
-- it must be used under the shared lock.
getPskChain
    :: MonadDBRead m
    => StakeholderId -> m DlgMemPool
getPskChain = getPskChainInternal HS.empty

-- See doc for 'getPskChain'. This function also stops traversal if
-- encounters anyone in 'toIgnore' set. This may be used to call it
-- several times to collect a whole tree/forest, for example.
getPskChainInternal
    :: MonadDBRead m
    => HashSet StakeholderId -> StakeholderId -> m DlgMemPool
getPskChainInternal toIgnore issuer =
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
isIssuerByAddressHash :: MonadDBRead m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDlgTransitive :: MonadDBRead m => StakeholderId -> m (Maybe PublicKey)
getDlgTransitive issuer = gsGetBi (transDlgKey issuer)

-- | Retrieves last PSK in chain of delegation started by public key
-- and resolves the passed issuer to a public key. Doesn't check that
-- user himself didn't delegate.
getDlgTransPsk
    :: MonadDBRead m
    => StakeholderId -> m (Maybe (PublicKey, ProxySKHeavy))
getDlgTransPsk issuer = getDlgTransitive issuer >>= \case
    Nothing -> pure Nothing
    Just dPk -> do
        chain <- HM.elems <$> getPskChain issuer
        let finalPsk = find (\psk -> pskDelegatePk psk == dPk) chain
        let iPk = pskIssuerPk <$>
                  find (\psk -> addressHash (pskIssuerPk psk) == issuer) chain
        let throwEmpty = throwM $ DBMalformed $
                sformat ("getDlgTransPk: couldn't find psk with dlgPk "%build%
                         " and issuer "%build)
                        dPk issuer
        maybe throwEmpty (pure . Just) $ (,) <$> iPk <*> finalPsk

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBRead m => PublicKey -> m (HashSet PublicKey)
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

-- | Converts heavy psk to the psk mapping action.
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
    :: (MonadDBRead m)
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
    deriving (Show)

instance RocksBatchOp DelegationOp where
    toBatchOp (PskFromEdgeAction (DlgEdgeAdd psk))
        | isRevokePsk psk =
          error $ "RocksBatchOp DelegationOp: malformed " <>
                  "revoke psk in DlgEdgeAdd: " <> pretty psk
        | not (verifyProxySecretKey psk) =
          error $ "Tried to insert invalid psk: " <> pretty psk
        | otherwise =
          [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk) (encodeStrict psk)]
    toBatchOp (PskFromEdgeAction (DlgEdgeDel issuerPk)) =
        [Rocks.Del $ pskKey $ addressHash issuerPk]
    toBatchOp (AddTransitiveDlg iPk dPk) =
        [Rocks.Put (transDlgKey $ addressHash iPk) (encodeStrict dPk)]
    toBatchOp (DelTransitiveDlg iPk) =
        [Rocks.Del $ transDlgKey $ addressHash iPk]
    toBatchOp (SetTransitiveDlgRev dPk iPks)
        | HS.null iPks = [Rocks.Del $ transRevDlgKey dPk]
        | otherwise    = [Rocks.Put (transRevDlgKey dPk) (encodeStrict iPks)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- Transitive relation iteration
data DlgTransIter

instance DBIteratorClass DlgTransIter where
    type IterKey DlgTransIter = StakeholderId
    type IterValue DlgTransIter = PublicKey
    iterKeyPrefix _ = iterTransPrefix

runDlgTransIterator
    :: forall m a . MonadRealDB m
    => DBnIterator DlgTransIter a -> m a
runDlgTransIterator = runDBnIterator @DlgTransIter _gStateDB

runDlgTransMapIterator
    :: forall v m a . MonadRealDB m
    => DBnMapIterator DlgTransIter v a -> (IterType DlgTransIter -> v) -> m a
runDlgTransMapIterator = runDBnMapIterator @DlgTransIter _gStateDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey s = "d/p/" <> encodeStrict s

transDlgKey :: StakeholderId -> ByteString
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
getDelegators :: MonadRealDB m => m (HashMap StakeholderId [StakeholderId])
getDelegators = runDlgTransMapIterator (step mempty) identity
  where
    step hm = nextItem >>= maybe (pure hm) (\(iss, addressHash -> del) -> do
        let curList = HM.lookupDefault [] del hm
        step (HM.insert del (iss:curList) hm))
