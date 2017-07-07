{-# LANGUAGE ScopedTypeVariables #-}
module Pos.Binary.Cbor.Serialization
    ( serialize
    , serialize'
    , deserialize
    , deserialize'
    , deserializeOrFail
    , deserializeOrFail'
    , putCopyBi
    , getCopyBi
    ) where

import qualified Codec.CBOR.Read                  as CBOR.Read
import qualified Codec.CBOR.Write                 as CBOR.Write
import           Control.Exception                (throw)
import           Control.Monad.ST                 (ST, runST)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Internal    as BSL
import           Data.SafeCopy                    (Contained, contain, safeGet, safePut)
import qualified Data.Serialize                   as Cereal (Get, Put)
import           Data.Typeable                    (typeRep)

import           Pos.Binary.Cbor.Class
import           Universum

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BSL.ByteString' and is constructed
-- incrementally.
serialize :: Bi a => a -> BSL.ByteString
serialize = CBOR.Write.toLazyByteString . encode

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a strict 'BS.ByteString'.
serialize' :: Bi a => a -> BS.ByteString
serialize' = BSL.toStrict . serialize

-- | Deserialize a Haskell value from the external binary representation
-- (which must have been made using 'serialize' or related function).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
deserialize :: Bi a => BSL.ByteString -> a
deserialize bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return x
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BSL.Empty           -> k Nothing      >>= supplyAllInput BSL.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = throw exn

-- | Strict variant of 'deserialize'.
deserialize' :: Bi a => BS.ByteString -> a
deserialize' = deserialize . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
-- or get back a @'DeserialiseFailure'@.
deserializeOrFail :: Bi a => BSL.ByteString -> Either CBOR.Read.DeserialiseFailure a
deserializeOrFail bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done _ _ x) = return (Right x)
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BSL.Empty           -> k Nothing      >>= supplyAllInput BSL.Empty
    supplyAllInput _ (CBOR.Read.Fail _ _ exn) = return (Left exn)

-- | Strict variant of 'deserializeOrFail'.
deserializeOrFail' :: Bi a => BS.ByteString -> Either CBOR.Read.DeserialiseFailure a
deserializeOrFail' = deserializeOrFail . BSL.fromStrict

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi = contain . safePut . serialize

getCopyBi :: forall a. (Bi a, Typeable a) => Contained (Cereal.Get a)
getCopyBi = contain $ do
    bs <- safeGet
    case deserializeOrFail bs of
        Left err -> fail $ "getCopy@" ++ show (typeRep $ Proxy @a) <> ": " <> show err
        Right x  -> return x

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
