-- | Utility functions for Test.Pos.Client.Txp

module Test.Pos.Client.Txp.Util
       ( seedSize
       , generateAddressAux
       , generateRedeemAddressAux
       ) where

import           Universum
import           Unsafe     (unsafeFromJust)

import           Formatting (build, sformat, (%))

import           Pos.Core   (makePubKeyAddressBoot, makeRedeemAddress)
import           Pos.Crypto (RedeemSecretKey, SecretKey, deterministicKeyGen,
                             redeemDeterministicKeyGen)
import           Pos.Types  (Address)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

seedSize :: Int
seedSize = 32

generateAddressAux :: ByteString -> (SecretKey, Address)
generateAddressAux seed =
    if length seed /= seedSize then
        error $ sformat ("Internal error: seed size must be exactly "%build%" bytes") seedSize
    else
        let (pk, sk) = deterministicKeyGen seed
        in (sk, makePubKeyAddressBoot pk)

generateRedeemAddressAux :: ByteString -> (RedeemSecretKey, Address)
generateRedeemAddressAux seed =
    if length seed /= seedSize then
        error $ sformat ("Internal error: seed size must be exactly "%build%" bytes") seedSize
    else
        let (pk, sk) = unsafeFromJust $ redeemDeterministicKeyGen seed
        in (sk, makeRedeemAddress pk)
