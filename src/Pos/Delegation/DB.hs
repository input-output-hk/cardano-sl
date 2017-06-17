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
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse

       , DelegationOp (..)

       , runDlgTransIterator
       , runDlgTransMapIterator

       , getDelegators
       ) where

import           Universum

import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import qualified Database.RocksDB          as Rocks

import           Pos.Binary.Class          (encodeStrict)
import           Pos.Crypto                (PublicKey, pskIssuerPk, verifyProxySecretKey)
import           Pos.DB                    (RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.Class              (MonadDBRead, MonadRealDB)
import           Pos.DB.GState.Common      (gsGetBi)
import           Pos.DB.Iterator           (DBIteratorClass (..), DBnIterator,
                                            DBnMapIterator, IterType, runDBnIterator,
                                            runDBnMapIterator)
import           Pos.DB.Types              (NodeDBs (_gStateDB))
import           Pos.Delegation.Cede.Types (DlgEdgeAction (..))
import           Pos.Delegation.Helpers    (isRevokePsk)
import           Pos.Types                 (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Util.Iterator         (nextItem)

----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer public key or his
-- address/stakeholder id, if present.
getPskByIssuer
    :: MonadDBRead m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPskByIssuer (either addressHash identity -> issuer) = gsGetBi (pskKey issuer)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDBRead m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i->x1->...->xn->d@.
getDlgTransitive :: MonadDBRead m => StakeholderId -> m (Maybe PublicKey)
getDlgTransitive issuer = gsGetBi (transDlgKey issuer)

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBRead m => PublicKey -> m (HashSet PublicKey)
getDlgTransitiveReverse dPk = fromMaybe mempty <$> gsGetBi (transRevDlgKey dPk)

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
