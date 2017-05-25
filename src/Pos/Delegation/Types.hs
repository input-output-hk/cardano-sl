{-# LANGUAGE TemplateHaskell #-}

-- | Delegation-related local types.

module Pos.Delegation.Types
       ( DlgPayload
       , DlgUndo
       , DlgMemPool
       , ProxySKLightConfirmation
       ) where

import           Universum

import           Pos.Core   (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Crypto (PublicKey)


-- | Delegation payload of the main block. The order of proxy sks
-- doesn't matter though, as it's checked for loops after application
-- all at once.
type DlgPayload = [ProxySKHeavy]

-- | PSKs we've overwritten/deleted.
type DlgUndo = [ProxySKHeavy]

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Confirmation of light cert type.
type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)
