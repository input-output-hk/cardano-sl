{-# LANGUAGE TypeFamilies #-}

-- | Part of GState DB which stores data necessary for heavyweight
-- delegation.
--
-- It stores the following mappings:
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
--
-- 4. ThisEpochPosted: Issuer → Bool. This mapping is used to check
-- whether stakeholder can post psk (he can't if he had posted this
-- epoch before).

module Pos.Delegation.DB
       (
         -- * Getters and predicates
         getPskByIssuer
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse
       , isIssuerPostedThisEpoch

         -- * Initialization
       , initGStateDlg

         -- * Batch ops
       , DelegationOp (..)

         -- * Iteration
       , DlgTransRevIter
       , getDelegators
       , getThisEpochPostedKeys
       ) where

import           Universum

import           Control.Lens (at, non)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (Source, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Database.RocksDB as Rocks

import           Pos.Binary.Class (serialize')
import           Pos.Core (HasConfiguration, ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Core.Genesis (GenesisDelegation (..))
import           Pos.Crypto (ProxySecretKey (..), PublicKey)
import           Pos.DB (RocksBatchOp (..), dbSerializeValue, encodeWithKeyPrefix)
import           Pos.DB.Class (DBIteratorClass (..), DBTag (..), MonadDB, MonadDBRead (..))
import           Pos.DB.GState.Common (gsGetBi, writeBatchGState)
import           Pos.Delegation.Cede.Types (DlgEdgeAction (..))
import           Pos.Delegation.Helpers (isRevokePsk)

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
-- @i→x1→...→xn→d@.
getDlgTransitive :: MonadDBRead m => StakeholderId -> m (Maybe StakeholderId)
getDlgTransitive issuer = gsGetBi (transDlgKey issuer)

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBRead m => StakeholderId -> m (HashSet StakeholderId)
getDlgTransitiveReverse dPk = fromMaybe mempty <$> gsGetBi (transRevDlgKey dPk)

-- | Check if issuer belongs to the postedThisEpoch map. See module
-- documentation for the details.
isIssuerPostedThisEpoch :: MonadDBRead m => StakeholderId -> m Bool
isIssuerPostedThisEpoch sId = do
    (r :: Maybe ()) <- gsGetBi (postedThisEpochKey sId)
    pure $ isJust r

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Initialize delegation part of GState DB.
initGStateDlg :: (MonadDB m) => GenesisDelegation -> m ()
initGStateDlg (unGenesisDelegation -> genesisDlg) =
    writeBatchGState $
    concat
        [pskOperations, transOperations, reverseOperations, thisEpochOperations]
  where
    stIdPairs :: [(StakeholderId, StakeholderId)] -- (issuer, delegate)
    stIdPairs =
        HM.toList genesisDlg <&> \(issuer, psk) ->
            (issuer, addressHash (pskDelegatePk psk))
    -- DB is split into 4 groups, each of them is represented as a
    -- list of operations.
    pskOperations = map (PskFromEdgeAction . DlgEdgeAdd) $ toList genesisDlg
    -- Delegates must not be issuers, so transitive closure is simple.
    transOperations = map (uncurry AddTransitiveDlg) stIdPairs
    reverseOperations =
        map (uncurry SetTransitiveDlgRev) $ HM.toList $
        foldl' revStep mempty stIdPairs
    revStep ::
           HashMap StakeholderId (HashSet StakeholderId)
        -> (StakeholderId, StakeholderId)
        -> HashMap StakeholderId (HashSet StakeholderId)
    revStep res (issuer, delegate) =
        res & at delegate . non mempty . at issuer .~ Just ()
    -- We assume that genesis delegation happened before 0-th epoch.
    thisEpochOperations = []

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = PskFromEdgeAction !DlgEdgeAction
    -- ^ Adds or removes Psk. Overwrites on addition if present.
    | AddTransitiveDlg !StakeholderId !StakeholderId
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg !StakeholderId
    -- ^ Remove i -> d link for i.
    | SetTransitiveDlgRev !StakeholderId !(HashSet StakeholderId)
    -- ^ Set value to map d -> [i], reverse index of transitive dlg
    | AddPostedThisEpoch !StakeholderId
    -- ^ Indicate that stakeholder has already posted psk this epoch.
    | DelPostedThisEpoch !StakeholderId
    -- ^ Remove stakeholderId from postedThisEpoch map.
    deriving (Show)

instance HasConfiguration => RocksBatchOp DelegationOp where
    toBatchOp (PskFromEdgeAction (DlgEdgeAdd psk))
        | isRevokePsk psk =
          error $ "RocksBatchOp DelegationOp: malformed " <>
                  "revoke psk in DlgEdgeAdd: " <> pretty psk
        | otherwise =
          [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk) (dbSerializeValue psk)]
    toBatchOp (PskFromEdgeAction (DlgEdgeDel issuerPk)) =
        [Rocks.Del $ pskKey issuerPk]
    toBatchOp (AddTransitiveDlg iSId dSId) =
        [Rocks.Put (transDlgKey iSId) (dbSerializeValue dSId)]
    toBatchOp (DelTransitiveDlg sId) =
        [Rocks.Del $ transDlgKey sId]
    toBatchOp (SetTransitiveDlgRev dSId iSIds)
        | HS.null iSIds = [Rocks.Del $ transRevDlgKey dSId]
        | otherwise     = [Rocks.Put (transRevDlgKey dSId) (dbSerializeValue iSIds)]
    toBatchOp (AddPostedThisEpoch sId) =
        [Rocks.Put (postedThisEpochKey sId) (dbSerializeValue ())]
    toBatchOp (DelPostedThisEpoch sId) =
        [Rocks.Del (postedThisEpochKey sId)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- Transitive relation iteration
data DlgTransRevIter

instance DBIteratorClass DlgTransRevIter where
    type IterKey DlgTransRevIter = StakeholderId
    type IterValue DlgTransRevIter = HashSet StakeholderId
    iterKeyPrefix = iterTransRevPrefix

-- | For each stakeholder, say who has delegated to that stakeholder
-- (basically iterate over transitive reverse relation).
--
-- NB. It's not called @getIssuers@ because we already have issuers (i.e.
-- block issuers)
getDelegators :: MonadDBRead m => Source (ResourceT m) (StakeholderId, HashSet StakeholderId)
getDelegators = dbIterSource GStateDB (Proxy @DlgTransRevIter)

-- Iterator over the "this epoch posted" set.
data ThisEpochPostedIter

instance DBIteratorClass ThisEpochPostedIter where
    type IterKey ThisEpochPostedIter = StakeholderId
    type IterValue ThisEpochPostedIter = ()
    iterKeyPrefix = iterPostedThisEpochPrefix

-- | Get all keys of thisEpochPosted set.
getThisEpochPostedKeys :: MonadDBRead m => m (HashSet StakeholderId)
getThisEpochPostedKeys =
    runConduitRes $
    mapOutput fst (dbIterSource GStateDB (Proxy @ThisEpochPostedIter)) .| consumeHs
  where
    consumeHs = CL.fold (flip HS.insert) mempty

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey s = "d/p/" <> serialize' s

transDlgKey :: StakeholderId -> ByteString
transDlgKey s = "d/t/" <> serialize' s

iterTransRevPrefix :: ByteString
iterTransRevPrefix = "d/tr/"

transRevDlgKey :: StakeholderId -> ByteString
transRevDlgKey = encodeWithKeyPrefix @DlgTransRevIter

iterPostedThisEpochPrefix :: ByteString
iterPostedThisEpochPrefix = "d/e/"

postedThisEpochKey :: StakeholderId -> ByteString
postedThisEpochKey = encodeWithKeyPrefix @ThisEpochPostedIter
