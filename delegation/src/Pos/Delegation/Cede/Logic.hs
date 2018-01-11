{-# LANGUAGE RankNTypes #-}

-- | Different logic depending on "MonadCede".

module Pos.Delegation.Cede.Logic
       (
         getPskChain
       , getPskChainInternal
       , detectCycleOnAddition
       , dlgReachesIssuance
       , dlgVerifyHeader
       , CheckForCycle(..)
       , dlgVerifyPskHeavy
       ) where

import           Universum

import           Control.Lens (uses, (%=))
import           Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Formatting (build, sformat, (%))

import           Pos.Core (EpochIndex, ProxySKHeavy, StakeholderId, addressHash, gbhConsensus)
import           Pos.Core.Block (BlockSignature (..), MainBlockHeader, mainHeaderLeaderKey,
                                 mcdSignature)
import           Pos.Crypto (ProxySecretKey (..), PublicKey, psigPsk)
import           Pos.DB (DBError (DBMalformed))
import           Pos.Delegation.Cede.Class (MonadCedeRead (..), getPskPk)
import           Pos.Delegation.Helpers (isRevokePsk)
import           Pos.Delegation.Types (DlgMemPool)
import           Pos.Lrc.Types (RichmenSet)

-- | Given an issuer, retrieves all certificate chains starting in
-- issuer. This function performs a series of sequential db reads so
-- it must be used under the shared lock.
getPskChain
    :: MonadCedeRead m
    => StakeholderId -> m DlgMemPool
getPskChain = getPskChainInternal HS.empty

-- See doc for 'getPskChain'. This function also stops traversal if
-- encounters anyone in 'toIgnore' set. This may be used to call it
-- several times to collect a whole tree/forest, for example.
getPskChainInternal
    :: MonadCedeRead m
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
        pskM <- getPsk x
        whenJust pskM $ \psk -> do
            when (isRevokePsk psk) $ throwM $ DBMalformed $
                "getPskChainInternal: found redeem psk: " <> pretty psk
            _1 %= HM.insert (pskIssuerPk psk) psk
            trav (addressHash $ pskDelegatePk psk)

-- | Checks if addition of the PSK to the map will lead to cycles. The
-- initial map may or may not contain this PSK. Returns nothing if
-- it's good, first-already-visited public key otherwise.
detectCycleOnAddition
    :: forall m . (MonadCedeRead m)
    => ProxySKHeavy                      -- ^ PSK to check against
    -> m (Maybe PublicKey)               -- ^ Problematic PSK if any
detectCycleOnAddition toAdd
    -- deleting (revoking) can't add a cycle
    | isRevokePsk toAdd = pure Nothing
    | otherwise = evalStateT (trav (pskDelegatePk toAdd))
                             (HS.singleton $ pskIssuerPk toAdd)
  where
    trav :: PublicKey -> StateT (HashSet PublicKey) m (Maybe PublicKey)
    trav cur = ifM (uses identity $ HS.member cur) (pure $ Just cur) $ do
        next <- lift $ getPsk $ addressHash cur
        identity %= HS.insert cur
        let stop = pure Nothing
        let panicRevoke p = error $ "dlgMemPoolDetectCycle: found revoke psk: " <> pretty p
        maybe stop (\psk -> bool (trav $ pskDelegatePk psk)
                                 (panicRevoke psk)
                                 (isRevokePsk psk))
                   next

-- | Given a psk getPskr, issuer, delegate and cert he uses (to sign,
-- or taken from psk), checks if there's a psk chain "issuer →
-- delegate" and the last cert matches the provided one (can be
-- retrieved using 'psigPsk'). This *does not* check that delegate
-- didn't issue a psk to somebody else.
dlgReachesIssuance
    :: (MonadCedeRead m)
    => PublicKey                             -- ^ Issuer
    -> PublicKey                             -- ^ Delegate
    -> ProxySKHeavy                          -- ^ i->d psk
    -> m Bool
dlgReachesIssuance i d _ | i == d = pure True
dlgReachesIssuance i d psk = reach i
  where
    -- Delegate 'd' has right to issue block instead of issuer 'i' if
    -- there's a delegation chain:
    --
    -- i → x₁ → x₂ → … xₖ → d
    --
    -- where every arrow is resolved psk, and the last one xₖ → d
    -- equals to the passed one.
    reach curUser = getPsk (addressHash curUser) >>= \case
        Nothing   -> pure False
        Just psk' -> let delegate = pskDelegatePk psk'
                     in  if delegate == d
                             then pure $ psk' == psk
                             else reach (pskDelegatePk psk')


-- | Verifies a header from delegation perspective (signature checks).
dlgVerifyHeader ::
       (MonadCedeRead m)
    => MainBlockHeader
    -> ExceptT Text m ()
dlgVerifyHeader h = do
    -- Issuer didn't delegate the right to issue to elseone.
    let issuer = h ^. mainHeaderLeaderKey
    let sig = h ^. gbhConsensus . mcdSignature
    issuerPsk <- getPskPk issuer
    whenJust issuerPsk $ \psk -> case sig of
        (BlockSignature _) ->
            throwError $
            sformat ("issuer "%build%" has delegated issuance right, "%
                     "so he can't issue the block, psk: "%build%", sig: "%build)
                issuer psk sig
        _ -> pass

    -- Check that if proxy sig is used, delegate indeed has right to
    -- issue the block. Signatures themselves are checked in the
    -- constructor, here we only verify they are related to slot
    -- leader. Self-signed proxySigs are forbidden on block
    -- construction level.
    case h ^. gbhConsensus ^. mcdSignature of
        (BlockPSignatureHeavy pSig) -> do
            let psk = psigPsk pSig
            let delegate = pskDelegatePk psk
            canIssue <- dlgReachesIssuance issuer delegate psk
            unless canIssue $ throwError $
                sformat ("heavy proxy signature's "%build%" "%
                         "related proxy cert can't be found/doesn't "%
                         "match the one in current allowed heavy psks set")
                        pSig
        (BlockPSignatureLight pSig) -> do
            let pskIPk = pskIssuerPk (psigPsk pSig)
            unless (pskIPk == issuer) $ throwError $
                sformat ("light proxy signature's "%build%" issuer "%
                         build%" doesn't match block slot leader "%build)
                        pSig pskIPk issuer
        _ -> pass

-- | Wrapper that turns on/off check of cycle creation in psk
-- verification.
newtype CheckForCycle = CheckForCycle Bool

-- | Verify consistent heavy PSK.
dlgVerifyPskHeavy ::
       (MonadCedeRead m)
    => RichmenSet
    -> CheckForCycle
    -> EpochIndex
    -> ProxySKHeavy
    -> ExceptT Text m ()
dlgVerifyPskHeavy richmen (CheckForCycle checkCycle) tipEpoch psk = do
    let iPk = pskIssuerPk psk
    let dPk = pskDelegatePk psk
    let stakeholderId = addressHash iPk

    -- Issuers have enough money (though it's free to revoke).
    when (not (isRevokePsk psk) &&
          not (stakeholderId `HS.member` richmen)) $
        throwError $ sformat
            ("PSK can't be accepted: issuer doesn't have enough stake: "%build)
            psk

    -- There are no psks that are isomorphic to ones from
    -- db. That is, if i delegated to d using psk1, we forbid
    -- using psk2 (in the next epoch) if psk2 delegates i → d
    -- as well.
    prevPsk <- getPsk stakeholderId
    let duplicate = do
            psk2 <- prevPsk
            guard $ pskIssuerPk psk2 == iPk && pskDelegatePk psk2 == dPk
            pure psk2
    whenJust duplicate $ \psk2 ->
        throwError $ sformat
            ("User effectively duplicates his previous PSK: "%
             build%" with new one "%build)
            psk2 psk

    -- No issuer has posted psk this epoch before (unless
    -- processed psk is a revocation).
    alreadyPostedThisEpoch <- hasPostedThisEpoch stakeholderId
    when (not (isRevokePsk psk) && alreadyPostedThisEpoch) $
        throwError $ sformat
            ("PSK "%build%" is not a revocation and his issuer "%
             " has already published psk this epoch")
            psk

    -- Every revoking psk indeed revokes previous non-revoking
    -- psk.
    when (isRevokePsk psk && isNothing prevPsk) $
        throwError $ sformat
            ("Revoke PSK "%build%" doesn't revoke anything")
            psk

    -- Internal PSK epoch should match current tip epoch.
    unless (tipEpoch == pskOmega psk) $
        throwError $ sformat
            ("PSK "%build%" has epoch which is different from tip epoch "%build)
            psk tipEpoch

    -- No cycle is created. This check is optional because when
    -- applying blocks we want to check for cycles after bulk
    -- application.
    when checkCycle $
        whenJustM (detectCycleOnAddition psk) $ \cyclePoint ->
            throwError $ sformat
                ("Adding PSK "%build%" leads to cycle at point "%build)
                psk cyclePoint
