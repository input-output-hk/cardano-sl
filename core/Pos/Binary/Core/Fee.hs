{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import           Data.Fixed       (Fixed (..))

import           Pos.Binary.Class (Bi (..), FixedSizeInt (..), PokeWithSize,
                                   convertToSizeNPut, getWithLength, getWord8, label,
                                   labelS, putField, putS, putWithLengthS, putWord8S)
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
        policyVersion <- getWord8
        getWithLength $ const $ case policyVersion of
            0 -> TxFeePolicyTxSizeLinear <$> get
            _ -> TxFeePolicyUnknown policyVersion <$> get
    sizeNPut = labelS "TxFeePolicy" $ convertToSizeNPut toBi
      where
        toBi :: TxFeePolicy -> PokeWithSize ()
        toBi = \case
            TxFeePolicyTxSizeLinear tsp ->
                putWord8S 0 *>
                putWithLengthS (putS tsp)
            TxFeePolicyUnknown v bs ->
                putWord8S v *>
                putWithLengthS (putS bs)
