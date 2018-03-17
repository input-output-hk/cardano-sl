-- | Some auxiliary crypto types
module UTxO.Crypto (
    -- * Key-pairs
    RegularKeyPair(..)
  , EncKeyPair(..)
  , RedeemKeyPair(..)
  , regularKeyPair
  , encKeyPair
  , encToRegular
    -- * Abstract API
  , SomeKeyPair(..)
  , TxOwnedInput
  , ClassifiedInputs(..)
  , classifyInputs
    -- * Delegation
  , DelegatedTo(..)
  ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Universum

import           Pos.Core
import           Pos.Crypto

{-------------------------------------------------------------------------------
  Keypairs
-------------------------------------------------------------------------------}

data RegularKeyPair = RegularKeyPair {
      regKpSec  :: SecretKey
    , regKpPub  :: PublicKey
    , regKpHash :: AddressHash PublicKey
    }
  deriving (Show)

data EncKeyPair = EncKeyPair {
      encKpEnc  :: EncryptedSecretKey
    , encKpSec  :: SecretKey
    , encKpPub  :: PublicKey
    , encKpHash :: AddressHash PublicKey
    }
  deriving (Show)

data RedeemKeyPair = RedeemKeyPair {
      redKpSec :: RedeemSecretKey
    , redKpPub :: RedeemPublicKey
    }
  deriving (Show)

regularKeyPair :: SecretKey -> RegularKeyPair
regularKeyPair regKpSec = RegularKeyPair {..}
  where
    regKpPub  = toPublic    regKpSec
    regKpHash = addressHash regKpPub

encKeyPair :: EncryptedSecretKey -> EncKeyPair
encKeyPair encKpEnc = EncKeyPair {..}
  where
    encKpSec  = encToSecret encKpEnc
    encKpPub  = encToPublic encKpEnc
    encKpHash = addressHash encKpPub

encToRegular :: EncKeyPair -> RegularKeyPair
encToRegular EncKeyPair{..} = RegularKeyPair{..}
  where
    regKpSec  = encKpSec
    regKpPub  = encKpPub
    regKpHash = encKpHash

{-------------------------------------------------------------------------------
  Abstract over the various kinds of keypairs we have
-------------------------------------------------------------------------------}

data SomeKeyPair =
    KeyPairRegular RegularKeyPair
  | KeyPairEncrypted EncKeyPair
  | KeyPairRedeem RedeemKeyPair

-- | An input to a transaction together with evidence that it's yours
--
-- (This is the singular form of 'TxOwnedInputs', which is defined in the
-- Cardano core libraries.)
type TxOwnedInput a = (a, TxIn)

data ClassifiedInputs =
    -- | This is regular set of inputs
    InputsRegular [TxOwnedInput RegularKeyPair]

    -- | When redeeming from an AVVM address, we can only have a single input
  | InputsRedeem (TxOwnedInput RedeemKeyPair)

-- | Classify a set of inputs
--
-- Maybe return an error message if the transaction contains an invalid
-- combination of inputs.
classifyInputs :: [TxOwnedInput SomeKeyPair] -> Either Text ClassifiedInputs
classifyInputs = \case
    []     -> Left "No inputs"
    (i:is) -> case classifyInput i of
                Left  i' -> go (InputsRegular [i']) is
                Right i' -> go (InputsRedeem   i' ) is
  where
    go :: ClassifiedInputs -> [TxOwnedInput SomeKeyPair] -> Either Text ClassifiedInputs
    go acc []                     = Right $ reverseAcc acc
    go (InputsRedeem _)    (_:_)  = Left "Can only have a single redemption input"
    go (InputsRegular acc) (i:is) =
        case classifyInput i of
          Left i' -> go (InputsRegular (i':acc)) is
          Right _ -> Left "Cannot mix redemption inputs with other inputs"


    classifyInput :: TxOwnedInput SomeKeyPair
                  -> Either (TxOwnedInput RegularKeyPair)
                            (TxOwnedInput RedeemKeyPair)
    classifyInput (KeyPairRegular   kp, inp) = Left (kp, inp)
    classifyInput (KeyPairEncrypted kp, inp) = Left (encToRegular kp, inp)
    classifyInput (KeyPairRedeem    kp, inp) = Right (kp, inp)

    reverseAcc :: ClassifiedInputs -> ClassifiedInputs
    reverseAcc (InputsRegular inps) = InputsRegular $ reverse inps
    reverseAcc (InputsRedeem  inp)  = InputsRedeem  $ inp

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

instance Buildable RegularKeyPair where
  build RegularKeyPair{..} = bprint
      ( "RegularKeyPair"
      % "{ sec:  " % build
      % ", pub:  " % build
      % ", hash: " % build
      % "}"
      )
      regKpSec
      regKpPub
      regKpHash

instance Buildable EncKeyPair where
  build EncKeyPair{..} = bprint
      ( "EncKeyPair"
      % "{ sec:  " % build
      % ", pub:  " % build
      % ", hash: " % build
      % "}"
      )
      encKpSec
      encKpPub
      encKpHash

instance Buildable RedeemKeyPair where
  build RedeemKeyPair{..} = bprint
      ( "RedeemKeyPair"
      % "{ sec: " % build
      % ", pub: " % build
      % "}"
      )
      redKpSec
      redKpPub

instance Buildable a => Buildable (DelegatedTo a) where
  build DelegatedTo{..} = bprint
      ( "DelegatedTo"
      % "{ to:  " % build
      % ", psk: " % build
      % "}"
      )
      delTo
      delPSK
