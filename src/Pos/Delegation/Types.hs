{-# LANGUAGE TemplateHaskell #-}

-- | Delegation-related network and local types.

module Pos.Delegation.Types
       (
       -- if you uncomment these, also uncomment tests
       -- in Test.Pos.Communication.Identity.BinarySpec
       --, CheckProxySKConfirmed (..)
       --, CheckProxySKConfirmedRes (..)

         DlgPayload
       ) where

import           Pos.Core (ProxySKHeavy)

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

type DlgPayload = [ProxySKHeavy]

----------------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------------

--derive makeArbitrary ''CheckProxySKConfirmed
--derive makeArbitrary ''CheckProxySKConfirmedRes
