{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import           Data.Fixed       (Fixed (..))

import           Pos.Binary.Class (Bi (..), FixedSizeInt (..), getRemainingByteString,
                                   getWithLength, getWord8, label, putByteString,
                                   putWithLength, putWord8)
import           Pos.Core.Fee     (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance Bi Coeff where
    get = label "Coeff" $ do
        FixedSizeInt (a :: Int64) <- get
        return (Coeff . MkFixed . fromIntegral $ a)
    put (Coeff (MkFixed a)) = do
        put $ FixedSizeInt (fromIntegral a :: Int64)

instance Bi TxSizeLinear where
    get = label "TxSizeLinear" $
        TxSizeLinear <$> get <*> get
    put (TxSizeLinear a b) = do
        put a
        put b

instance Bi TxFeePolicy where
    get = label "TxFeePolicy" $ do
        policyVersion <- getWord8
        getWithLength $ case policyVersion of
            0 -> TxFeePolicyTxSizeLinear <$> get
            _ -> do
                bs <- getRemainingByteString
                return $ TxFeePolicyUnknown policyVersion bs
    put = \case
        TxFeePolicyTxSizeLinear tsp -> do
            putWord8 0
            putWithLength $ put tsp
        TxFeePolicyUnknown v bs -> do
            putWord8 v
            putWithLength $ putByteString bs

