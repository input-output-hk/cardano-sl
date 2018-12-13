module Test.Spec.AddressPool (spec) where

import           Universum

import           Cardano.Wallet.Kernel.AddressPool (AddressPool,
                     getAddressPoolGap, getAddressPoolSize, initAddressPool,
                     lookupAddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)

import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Property, label, property, (.&&.), (===))

spec :: Spec
spec = describe "AddressPool" $ do
    it "lookupAddressPool" $ property prop_lookupAddressPool

-- | It proves that pool extension works as expected.
prop_lookupAddressPool
    :: (AddressPoolGap, Int)
    -> Property
prop_lookupAddressPool (gap, addr) = do
    prop_lookupAddressPool' (initAddressPool gap identity, fromIntegral addr)

prop_lookupAddressPool'
    :: (Show address, Ord address)
    => (AddressPool address, address)
    -> Property
prop_lookupAddressPool' (pool, addr) =
    case lookupAddressPool addr pool of
        (Nothing, pool') -> label "hit outside pool" $
            on (===) getAddressPoolSize pool pool' .&&.
            on (===) getAddressPoolGap  pool pool'
        (Just (addr', _), pool') -> label "hit within pool" $
            let k = on (-) getAddressPoolSize pool' pool
            in  addr' === addr .&&.
                on (===) getAddressPoolGap pool pool' .&&.
                property (k >= 0 && k <= fromIntegral (getAddressPoolGap pool))
