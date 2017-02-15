{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-} -- makeArbitrary uses error, we use panic

-- | Delegation-related network and local types.

module Pos.Delegation.Types
       ( SendProxySK (..)
       , ConfirmProxySK (..)
       -- if you uncomment these, also uncomment tests
       -- in Test.Pos.Communication.Identity.BinarySpec
       --, CheckProxySKConfirmed (..)
       --, CheckProxySKConfirmedRes (..)
       ) where

import           Data.DeriveTH   (derive, makeArbitrary)
import           Test.QuickCheck (Arbitrary (..), choose)
import           Universum

import           Pos.Types       (ProxySKHeavy, ProxySKLight, ProxySigLight)

----------------------------------------------------------------------------
-- Generic PSKs propagation
----------------------------------------------------------------------------

-- | Message with delegated proxy secret key. Is used to propagate
-- both epoch-oriented psks (lightweight) and simple (heavyweight).
data SendProxySK
    = SendProxySKLight !ProxySKLight
    | SendProxySKHeavy !ProxySKHeavy
    deriving (Show, Eq, Generic)

instance Hashable SendProxySK

----------------------------------------------------------------------------
-- Lightweight PSKs confirmation mechanism
----------------------------------------------------------------------------

-- | Confirmation of proxy signature delivery. Delegate should take
-- the proxy signing key he has and sign this key with itself. If the
-- signature is correct, then it was done by delegate (guaranteed by
-- PSK scheme). Checking @w@ can be done with @(const True)@
-- predicate, because certificate may be sent in epoch id that's
-- before lower cert's @EpochIndex@.
data ConfirmProxySK =
    ConfirmProxySK !ProxySKLight !(ProxySigLight ProxySKLight)
    deriving (Show, Eq, Generic)

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
-- Arbitrary instances
----------------------------------------------------------------------------

derive makeArbitrary ''SendProxySK
derive makeArbitrary ''ConfirmProxySK
--derive makeArbitrary ''CheckProxySKConfirmed
--derive makeArbitrary ''CheckProxySKConfirmedRes
