{-# LANGUAGE TemplateHaskell #-}

-- | Delegation-related network and local types.

module Pos.Delegation.Types
       (
       -- if you uncomment these, also uncomment tests
       -- in Test.Pos.Communication.Identity.BinarySpec
       --, CheckProxySKConfirmed (..)
       --, CheckProxySKConfirmedRes (..)
         DlgPayload
       , DlgMemPool
       , ProxySKLightConfirmation
       ) where

import           Universum

import           Pos.Core   (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Crypto (PublicKey)

type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)

---- | Request to check if a node has any info about PSK delivery.
--data CheckProxySKConfirmed =
--    CheckProxySKConfirmed !ProxySKLight
--    deriving (Show, Eq, Generic)
--
---- | Response to the @CheckProxySKConfirmed@ call.
--data CheckProxySKConfirmedRes =
--    CheckProxySKConfirmedRes !Bool
--    deriving (Show, Eq, Generic)

----------------------------------------------------------------------------
-- Heavyweight delegation payload
----------------------------------------------------------------------------

-- | Delegation payload of the main block.
type DlgPayload = [ProxySKHeavy]

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

----------------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------------

--derive makeArbitrary ''CheckProxySKConfirmed
--derive makeArbitrary ''CheckProxySKConfirmedRes
