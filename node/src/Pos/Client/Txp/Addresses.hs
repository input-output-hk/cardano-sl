{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Addresses
       ( MonadAddresses (..)
       ) where

import           Universum

import           Pos.Core  (Address, StakeholderId)

-- | A class which have the method to generate a new address
class Monad m => MonadAddresses m where
    type AddrData m :: *

    -- | Generate new address using given 'AddrData' (e.g. password +
    -- account id). It also optionally returns 'StakeholderId' which
    -- should own stake sent to this address. It's a temporary
    -- solution, because stake distribution is actually part of
    -- address, but it's not used. CSL-1489 is dedicated to it.
    getNewAddress :: AddrData m -> m (Address, Maybe StakeholderId)
