{-# LANGUAGE TemplateHaskell #-}

{-------------------------------------------------------------------------------

  The AddressPool module enables BIP44-style address and account discovery
  for Externally Owned Sequential (EOS) wallets.

  For EOS wallets, we can't add new accounts and therefore can't discover any
  accounts that were not provided upfront.

  In this style of address discovery we can make no assumptions about the sequence
  of accounts, we can only discover addresses in the given wallets (and indeed
  add new addresses to be discovered)

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.AddressPool
    ( AddressPool
    , initAddressPool
    , lookupAddressPool
    , getAddressPoolSize
    , getAddressPoolGap
    ) where

import           Universum

import           Control.Lens (at, makeLenses)
import qualified Data.Map as Map
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)


{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- Note: the account and address types are polymorphic in order to support
-- varying wallet types
data AddressPool address = AddressPool
    { _addresses -- all addresses currently in the pool, discovered or not
        :: Map address Word

    , _gap -- the "gap" to maintain after the last discovered address in a pool
        :: AddressPoolGap

    , _nextAddresses -- compute next 'gap' addresses from an index
        :: Word -> Map address Word
    }
makeLenses ''AddressPool

instance Buildable address => Buildable (AddressPool address) where
    build pool = bprint
        ("AddressPool (gap: "%int%") (total: "%int%") "%listJson)
        (pool ^. gap)
        (Map.size $ pool ^. addresses)
        (map (uncurry $ bprint (build%":"%build)) $ Map.toList $ pool ^. addresses)


{-------------------------------------------------------------------------------
  Usage
-------------------------------------------------------------------------------}

initAddressPool
    :: (Ord address)
    => AddressPoolGap
    -> (Word -> address)
    -> AddressPool address
initAddressPool gap_ newAddress = AddressPool
    { _addresses = nextAddresses_ 0
    , _gap = gap_
    , _nextAddresses = nextAddresses_
    }
  where
    nextAddresses_ fromIx =
        let
            toIx = fromIx + (fromIntegral gap_) - 1
        in
            invariant (toIx > fromIx) "initAddressPool: toIx should be greater than fromIx"
            $ Map.fromList
            $ map (\ix -> (newAddress ix, ix))
            [fromIx .. toIx]


-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended in the following ways
--
--   * if the address was discovered near the edge, the pool is extended
--
-- It is also possible that the pool is not amended at all - this happens in the
-- case that an address is discovered 'far' from the edge.
lookupAddressPool
    :: (Ord address)
    => address
    -> AddressPool address
    -> (Maybe (address, Word), AddressPool address)
lookupAddressPool target pool =
    case pool ^. (addresses . at target) of
        Just ix ->
            (Just (target, ix), extendAddressPool ix pool)

        Nothing ->
            (Nothing, pool)

-- | Get the underlying pool's size
getAddressPoolSize
    :: AddressPool address -> Int
getAddressPoolSize pool = Map.size $ pool ^. addresses

-- | Get the underlying pool's gap
getAddressPoolGap
    :: AddressPool address -> AddressPoolGap
getAddressPoolGap pool = pool ^. gap

{-------------------------------------------------------------------------------
  Internals
-------------------------------------------------------------------------------}

-- | If an address is discovered near the edge, we extend the address sequence,
--   otherwise we return the pool untouched.
extendAddressPool
    :: (Ord address)
    => Word
    -> AddressPool address
    -> AddressPool address
extendAddressPool ix pool
    | isOnEdge  = pool & addresses %~ (next <>)
    | otherwise = pool
  where
    edge = Map.size (pool ^. addresses)
    isOnEdge = fromIntegral edge - ix <= fromIntegral (pool ^. gap)
    next = (pool ^. nextAddresses) (ix + 1)


{-------------------------------------------------------------------------------
  Utils
-------------------------------------------------------------------------------}

-- | Fails hard if an invariant does not hold, or proceed.
invariant
    :: Bool
    -> Text
    -> a
    -> a
invariant predicate msg action =
    if predicate then action else error msg
