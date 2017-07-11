{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import           Data.Fixed       (Fixed (..), Nano)

import           Pos.Binary.Class (Bi (..), FixedSizeInt (..), PokeWithSize,
                                   convertToSizeNPut, getBytes, getWithLength, getWord8,
                                   label, labelS, putBytesS, putField, putS,
                                   putWithLengthS, putWord8S)
import           Pos.Core.Fee     (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

import qualified Pos.Binary.Cbor  as Cbor

instance Bi Coeff where
    get = label "Coeff" $ do
        FixedSizeInt (a :: Int64) <- get
        pure (Coeff . MkFixed . fromIntegral $ a)
    sizeNPut = labelS "Coeff" $
        putField $ \(Coeff (MkFixed a)) -> FixedSizeInt (fromIntegral a :: Int64)

instance Cbor.Bi Coeff where
  encode (Coeff n) = Cbor.encode n
  decode = Coeff <$> Cbor.decode @Nano

instance Bi TxSizeLinear where
    get = label "TxSizeLinear" $
        TxSizeLinear <$> get <*> get
    sizeNPut = labelS "TxSizeLinear" $
        putField (\(TxSizeLinear a _) -> a) <>
        putField (\(TxSizeLinear _ b) -> b)

instance Cbor.Bi TxSizeLinear where
  encode (TxSizeLinear a b) = Cbor.encodeListLen 2 <> Cbor.encode a <> Cbor.encode b
  decode = do
    Cbor.enforceSize "TxSizeLinear" 2
    !a <- Cbor.decode @Coeff
    !b <- Cbor.decode @Coeff
    return $ TxSizeLinear a b

instance Bi TxFeePolicy where
    get = label "TxFeePolicy" $ do
        tag <- getWord8
        getWithLength $ \len -> case tag of
            0 -> TxFeePolicyTxSizeLinear <$> get
            _ -> TxFeePolicyUnknown tag <$> getBytes (fromIntegral len)
    sizeNPut = labelS "TxFeePolicy" $ convertToSizeNPut $ \case
        TxFeePolicyTxSizeLinear tsp ->
            putWithTag 0 $ putS tsp
        TxFeePolicyUnknown t bs ->
            putWithTag t $ putBytesS bs
      where
        -- | Put tag, then length of X, then X itself
        putWithTag :: Word8 -> PokeWithSize () -> PokeWithSize ()
        putWithTag t x = putWord8S t <> putWithLengthS x

instance Cbor.Bi TxFeePolicy where
  encode policy = case policy of
    TxFeePolicyTxSizeLinear txSizeLinear -> Cbor.encodeListLen 2 <> Cbor.encode (0 :: Word8) <> Cbor.encode txSizeLinear
    TxFeePolicyUnknown word8 bs          -> Cbor.encodeListLen 2 <> Cbor.encode word8        <> Cbor.encode bs
  decode = do
    Cbor.enforceSize "TxFeePolicy" 2
    tag <- Cbor.decode @Word8
    case tag of
      0 -> TxFeePolicyTxSizeLinear <$> Cbor.decode @TxSizeLinear
      _ -> TxFeePolicyUnknown tag  <$> Cbor.decode
