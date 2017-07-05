{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import           Data.Fixed       (Fixed (..))

import           Pos.Binary.Class (Bi (..), FixedSizeInt (..), PokeWithSize,
                                   convertToSizeNPut, getBytes, getWithLength, getWord8,
                                   label, labelS, putBytesS, putField, putS,
                                   putWithLengthS, putWord8S)
import           Pos.Core.Fee     (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance Bi Coeff where
    get = label "Coeff" $ do
        FixedSizeInt (a :: Int64) <- get
        pure (Coeff . MkFixed . fromIntegral $ a)
    sizeNPut = labelS "Coeff" $
        putField $ \(Coeff (MkFixed a)) -> FixedSizeInt (fromIntegral a :: Int64)

instance Bi TxSizeLinear where
    get = label "TxSizeLinear" $
        TxSizeLinear <$> get <*> get
    sizeNPut = labelS "TxSizeLinear" $
        putField (\(TxSizeLinear a _) -> a) <>
        putField (\(TxSizeLinear _ b) -> b)

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
