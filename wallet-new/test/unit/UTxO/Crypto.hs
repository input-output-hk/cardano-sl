-- | Some auxiliary crypto types
module UTxO.Crypto (
    -- * Key-pairs
    KeyPair(..)
  , EncKeyPair(..)
  , keyPair
  , encKeyPair
  , fromEncKeyPair
    -- * Delegation
  , DelegatedTo(..)
  ) where

import Universum
import Formatting (bprint, build, (%))
import qualified Data.Text.Buildable

import Pos.Core
import Pos.Crypto

{-------------------------------------------------------------------------------
  Keypairs
-------------------------------------------------------------------------------}

data KeyPair = KeyPair {
      kpSec  :: SecretKey
    , kpPub  :: PublicKey
    , kpHash :: AddressHash PublicKey
    }
  deriving (Show)

data EncKeyPair = EncKeyPair {
      ekpEnc  :: EncryptedSecretKey
    , ekpSec  :: SecretKey
    , ekpPub  :: PublicKey
    , ekpHash :: AddressHash PublicKey
    }
  deriving (Show)

keyPair :: SecretKey -> KeyPair
keyPair kpSec = KeyPair {..}
  where
    kpPub  = toPublic    kpSec
    kpHash = addressHash kpPub

encKeyPair :: EncryptedSecretKey -> EncKeyPair
encKeyPair ekpEnc = EncKeyPair {..}
  where
    ekpSec  = encToSecret ekpEnc
    ekpPub  = encToPublic ekpEnc
    ekpHash = addressHash ekpPub

fromEncKeyPair :: EncKeyPair -> KeyPair
fromEncKeyPair EncKeyPair{..} = KeyPair{..}
  where
    kpSec  = ekpSec
    kpPub  = ekpPub
    kpHash = ekpHash

{-------------------------------------------------------------------------------
  Delegation
-------------------------------------------------------------------------------}

data DelegatedTo a = DelegatedTo {
      delTo  :: a
    , delPSK :: ProxySKHeavy
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable KeyPair where
  build KeyPair{..} = bprint
      ( "KeyPair"
      % "{ sec:  " % build
      % ", pub:  " % build
      % ", hash: " % build
      % "}"
      )
      kpSec
      kpPub
      kpHash

instance Buildable EncKeyPair where
  build EncKeyPair{..} = bprint
      ( "EncKeyPair"
      % "{ sec:  " % build
      % ", pub:  " % build
      % ", hash: " % build
      % "}"
      )
      ekpSec
      ekpPub
      ekpHash

instance Buildable a => Buildable (DelegatedTo a) where
  build DelegatedTo{..} = bprint
      ( "DelegatedTo"
      % "{ to:  " % build
      % ", psk: " % build
      % "}"
      )
      delTo
      delPSK
