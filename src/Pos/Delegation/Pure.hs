{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure functions on different delegation datatypes.

module Pos.Delegation.Pure
       ( isRevokePsk
       , dlgMemPoolApplyBlock
       , dlgMemPoolDetectCycle
       , dlgReachesIssuance
       ) where


import           Universum

import           Control.Lens         (uses, (%=))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.List            (partition)

import           Pos.Block.Core       (MainBlock, mainBlockDlgPayload)
import           Pos.Core             (EpochIndex, ProxySKHeavy)
import           Pos.Crypto           (ProxyCert, ProxySecretKey (..), PublicKey)
import           Pos.Delegation.Types (DlgMemPool, getDlgPayload)


-- | Checks if given PSK revokes delegation (issuer = delegate).
isRevokePsk :: ProxySKHeavy -> Bool
isRevokePsk ProxySecretKey{..} = pskIssuerPk == pskDelegatePk

-- | Applies block certificates to 'ProxySKHeavyMap'.
dlgMemPoolApplyBlock :: MainBlock ssc -> DlgMemPool -> DlgMemPool
dlgMemPoolApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition isRevokePsk (getDlgPayload $ block ^. mainBlockDlgPayload)
    for_ toDelete $ \psk -> identity %= HM.delete (pskIssuerPk psk)
    for_ toReplace $ \psk -> identity %= HM.insert (pskIssuerPk psk) psk

-- | Checks if addition of the PSK to the map will lead to cycles. The
-- initial map may or may not contain this PSK. Returns nothing if
-- it's good, first-already-visited public key otherwise.
dlgMemPoolDetectCycle
    :: forall m . (Monad m)
    => (PublicKey -> m (Maybe ProxySKHeavy)) -- ^ Resolving function
    -> ProxySKHeavy                          -- ^ PSK to check against
    -> m (Maybe PublicKey)
dlgMemPoolDetectCycle resolve toAdd =
    evalStateT (trav (pskDelegatePk toAdd)) (HS.singleton $ pskIssuerPk toAdd)
  where
    trav :: PublicKey -> StateT (HashSet PublicKey) m (Maybe PublicKey)
    trav cur = ifM (uses identity $ HS.member cur) (pure $ Just cur) $ do
        next <- lift $ resolve cur
        identity %= HS.insert cur
        let stop = pure Nothing
        maybe stop (\psk -> bool stop (trav $ pskDelegatePk psk) $ isRevokePsk psk) next

-- | Given an 'DlgMemPool', issuer, delegate and his psk, checks if
-- delegate is allowed to issue the block. This does not check that
-- delegate didn't issue a psk to somebody else.
dlgReachesIssuance
    :: (Monad m)
    => (PublicKey -> m (Maybe ProxySKHeavy)) -- ^ Resolving function (HM.lookup in pure case)
    -> PublicKey                             -- ^ Issuer
    -> PublicKey                             -- ^ Delegate
    -> ProxyCert EpochIndex                  -- ^ Proxy cert of i->d psk/psig
    -> m Bool
dlgReachesIssuance _ i d _ | i == d = pure True
dlgReachesIssuance resolve i d cert = reach i
  where
    -- Delegate 'd' has right to issue block instead of issuer 'i' if
    -- there's a delegation chain:
    --
    -- i → x₁ → x₂ → … xₖ → d
    --
    -- where every arrow is psk present in the 'pskm', and the last
    -- psk xₖ → d has the same certificate as 'cert'.
    reach curUser = resolve curUser >>= \case
        Nothing -> pure False
        Just ProxySecretKey{..}
            | pskDelegatePk == d -> pure $ pskCert == cert
            | otherwise          -> reach pskDelegatePk
