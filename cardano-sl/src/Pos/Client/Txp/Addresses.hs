{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Addresses
       ( MonadAddresses (..)
       ) where

import           Universum

import           Pos.Types (Address)

-- | A class which have the method to generate a new address
class Monad m => MonadAddresses m where
    type AddrData m :: *

    -- | Generate new address using given 'AddrData'
    -- (e.g. password + account id)
    getNewAddress :: AddrData m -> m Address
