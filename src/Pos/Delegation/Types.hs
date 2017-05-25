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


----------------------------------------------------------------------------
-- Heavyweight delegation payload
----------------------------------------------------------------------------

-- | Delegation payload of the main block.
type DlgPayload = [ProxySKHeavy]

-- | PSKs we've overwritten/deleted
type DlgUndo = [ProxySKHeavy]

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Confirmation of light cert type.
type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)
