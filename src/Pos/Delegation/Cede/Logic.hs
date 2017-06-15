{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different logic depending on "MonadCede".

module Pos.Delegation.Cede.Logic
       (
         getPskChain
       , getPskChainInternal
       , detectCycleOnAddition
       , dlgReachesIssuance
       ) where

import           Universum

import           Control.Lens              (uses, (%=))
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS

import           Pos.Core                  (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Crypto                (ProxySecretKey (..), PublicKey)
import           Pos.DB                    (DBError (DBMalformed))
import           Pos.Delegation.Cede.Class (MonadCedeRead (..))
import           Pos.Delegation.Helpers    (isRevokePsk)
import           Pos.Delegation.Types      (DlgMemPool)

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
        Nothing -> pure False
        Just psk'@ProxySecretKey{..}
            | pskDelegatePk == d -> pure $ psk' == psk
            | otherwise          -> reach pskDelegatePk
