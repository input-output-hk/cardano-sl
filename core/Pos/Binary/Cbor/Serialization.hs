module Pos.Binary.Cbor.Serialization
    ( serialize
    , deserialize
    , deserializeOrFail
    ) where

import qualified Codec.CBOR.Read                  as CBOR.Read
import qualified Codec.CBOR.Write                 as CBOR.Write
import           Control.Exception                (throw)
import           Control.Monad.ST                 (ST, runST)
import qualified Data.ByteString.Lazy             as BS
import qualified Data.ByteString.Lazy.Internal    as BS

import           Pos.Binary.Cbor.Class
import           Universum

-- | Serialise a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BS.ByteString' and is constructed
-- incrementally.
serialize :: Bi a => a -> BS.ByteString
serialize = CBOR.Write.toLazyByteString . encode

-- | Deserialize a Haskell value from the external binary representation
-- (which must have been made using 'serialise' or related function).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
deserialize :: Bi a => BS.ByteString -> a
deserialize bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return x
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BS.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BS.Empty           -> k Nothing      >>= supplyAllInput BS.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = throw exn

-- | Deserialize a Haskell value from the external binary representation,
-- or get back a @'DeserialiseFailure'@.
deserializeOrFail :: Bi a => BS.ByteString -> Either CBOR.Read.DeserialiseFailure a
deserializeOrFail bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return (Right x)
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BS.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BS.Empty           -> k Nothing      >>= supplyAllInput BS.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = return (Left exn)

----------------------------------------

-- | Deserialize a Haskell value from the external binary representation.
--
-- This allows /input/ data to be provided incrementally, rather than all in one
-- go. It also gives an explicit representation of deserialisation errors.
--
-- Note that the incremental behaviour is only for the input data, not the
-- output value: the final deserialized value is constructed and returned as a
-- whole, not incrementally.
deserializeIncremental :: Bi a => ST s (CBOR.Read.IDecode s a)
deserializeIncremental = CBOR.Read.deserialiseIncremental decode
