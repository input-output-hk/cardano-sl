module Delegation.Keys
  ( Owner(..)
  , SKey(..)
  , VKey(..)
  , KeyPair(..)
  , keyPair
  , HashKey(..)
  , Sig
  , hashKey
  , sign
  , verify
  ) where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS

-- |Representation of the owner of keypair.
newtype Owner = Owner Int deriving (Show, Eq, Ord)

-- |Private/Secret Key
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Public Key
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

-- |Key Pair
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord)

keyPair :: Owner -> KeyPair
keyPair owner = KeyPair (SKey owner) (VKey owner)

-- |The hash of public Key
newtype HashKey = HashKey (Digest SHA256) deriving (Show, Eq, Ord)

-- |A digital signature
data Sig a = Sig a Owner deriving (Show, Eq, Ord)

-- |Hash a given public key
hashKey :: VKey -> HashKey
hashKey key = HashKey $ hash key

-- |Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

