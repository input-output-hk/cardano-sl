{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight
-- delegation.
--
-- It stores three mappings:
--
-- 1. Psk mapping: Issuer → PSK, where pskIssuer of PSK is Issuer (must
-- be consistent). We don't store revokation psks, instead we just
-- delete previous Issuer → PSK. DB must not contain revokation psks.
--
-- 2. Dlg transitive mapping: Issuer → Delegate. This one is
-- transitive relation "i delegated to d through some chain of
-- certificates". DB must not contain cycles in psk mapping. As
-- mappings of kind I → I are forbidden (no revokation psks), Delegate
-- is always different from Issuer.
--
-- 3. Dlg reverse transitive mapping: Delegate →
-- Issuers@[Issuer]. Corresponds to Issuer → Delegate ∈ Dlg transitive
-- mapping. Notice: here also Delegate ∉ Issuers (see (2)).

module Pos.Delegation.DB
       ( getPskByIssuer
       , getPskChain
       , getPskForest
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse

       , DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer
       , DelegationOp (..)

       , runDlgTransIterator
       , runDlgTransMapIterator

       , getDelegators
       ) where

import           Universum

import           Control.Lens         (uses, (%=))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Database.RocksDB     as Rocks

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Crypto           (PublicKey, pskIssuerPk)
import           Pos.DB.Class         (MonadDB, MonadDBPure)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.DB.Iterator      (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                       IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types         (NodeDBs (_gStateDB))
import           Pos.Delegation.Pure  (isRevokePsk)
import           Pos.Delegation.Types (DlgMemPool)
import           Pos.Types            (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Util.Iterator    (nextItem)

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
-- encounters anyone in 'toIgnore' set.
getPskChainInternal
    :: MonadDBPure m
    => HashSet StakeholderId -> Either PublicKey StakeholderId -> m DlgMemPool
getPskChainInternal toIgnore (either addressHash identity -> issuer) =
    fmap (view _1) $ flip execStateT (HM.empty, [issuer], HS.empty) trav
  where
    trav = use _2 >>= \case
        []                           -> pass
        (x:_) | HS.member x toIgnore -> (_2 %= drop 1) >> trav
        (x:_)                        -> do
            whenM (uses _3 $ HS.member x) $
                throwM $ DBMalformed "getPskTree: found a PSK cycle"
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
    :: (MonadDBPure m)
    => Either [PublicKey] [StakeholderId] -> m DlgMemPool
getPskForest (either (fmap addressHash) identity -> issuers) =
    foldlM foldFoo HM.empty (map Right issuers)
  where
    -- Don't revisit branches we retrieved earlier.
    foldFoo cur = getPskChainInternal (HS.fromList $ map addressHash $ HM.keys cur)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDBPure m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDlgTransitive :: MonadDBPure m => PublicKey -> m (Maybe PublicKey)
getDlgTransitive iPk = gsGetBi (transDlgKey iPk)

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBPure m => PublicKey -> m [PublicKey]
getDlgTransitiveReverse dPk = fmap (fromMaybe []) $ gsGetBi (transRevDlgKey dPk)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !PublicKey
    deriving (Show, Eq, Generic)

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

data DelegationOp
    = PskFromEdgeAction !DlgEdgeAction
    -- ^ Adds or removes Psk. Overwrites on addition if present.
    | AddTransitiveDlg !PublicKey !PublicKey
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg !PublicKey
    -- ^ Remove i -> d link for i.
    | SetTransitiveDlgRev !PublicKey !([PublicKey])
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
