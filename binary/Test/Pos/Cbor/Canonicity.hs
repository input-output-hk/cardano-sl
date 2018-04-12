{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Cbor.Canonicity
    ( perturbCanonicity
    ) where

import           Universum

import qualified Control.Monad.State as S
import           GHC.Float (RealFloat (..))
import           Numeric.Half (Half (..))
import           Test.QuickCheck.Gen (Gen, choose, elements, oneof, shuffle, sized)

import           Test.Pos.Cbor.RefImpl (Term (..), UInt (..), toUInt, leadingZeroes,
                                        integerToBinaryRep, canonicalNaN)

-- | Traverse elements of a Term which can be represented in multiple ways and
-- apply appropriate functions to them. We assume that Term is obtained from
-- deserializing values serialized using 'Bi' class, hence certain terms will
-- not occur. Therefore we make additional effort to check for them and bail if
-- they are there.
--
-- Used in 'countCanonicalBits' for counting the number of canonical bits in a
-- term and 'perturbCanonicity' for modifying them randomly to obtain
-- non-canonical term.
traverseCanonicalBits
    :: Monad m
    => (UInt -> m Term)                         -- ^ positive int
    -> (UInt -> m Term)                         -- ^ negative int
    -> m Word8                                  -- ^ big integer (add leading zeroes)
    -> (UInt -> m UInt)                         -- ^ length of a string/collection
    -> (UInt -> m UInt)                         -- ^ tag
    -> (forall term. [term] -> m [term])        -- ^ map/set
    -> (Half -> m Half)                         -- ^ 16-bit NaN
    -> Term
    -> m Term
traverseCanonicalBits tuint tnint tzeroes tlength ttag tmapset tnan16 = go
  where
    go = \case
        -- Representation can be widened or changed to TBigInt.
        TUInt n    -> tuint n
        -- Representation can be widened or changed to TBigInt.
        TNInt n    -> tnint n
        -- Leading zeroes can be added to binary representation.
        TBigInt oldBs _len n -> case leadingZeroes oldBs of
            -- Canonical representation doesn't have leading zeroes.
            0 -> do
                bs <- integerToBinaryRep <$> tzeroes <*> pure n
                TBigInt <$> pure bs
                        <*> (tlength . toUInt . fromIntegral $ length bs)
                        <*> pure n
            k -> error $ "Unexpected TBigInt with " <> show k
                      <> " leading zeroes: " <> show n
        -- Representation of length can be widened.
        TBytes  len ws -> TBytes  <$> tlength len <*> pure ws
        TString len ws -> TString <$> tlength len <*> pure ws
        -- Representation of length can be widened, order of term pairs can be
        -- changed or duplicates added.
        TMap len terms -> mapM (\(k, v) -> (,) <$> go k <*> go v) terms
            >>= \terms' -> TMap <$> tlength len <*> tmapset terms'
        -- Order of terms can be changed or duplicates added.
        TTagged tag@(UInt16 258) (TArray len terms) -> do
            newTag <- ttag tag
            terms' <- mapM go terms
            TTagged newTag <$> (TArray <$> tlength len <*> tmapset terms')
        -- Representation of a NaN can be changed.
        TFloat16 f
            | not $ isNaN f -> pure $ TFloat16 f
            | getHalf f == getHalf canonicalNaN -> TFloat16 <$> tnan16 f
            | otherwise -> error "Unexpected 16bit representation of NaN"
        -- Tag representation can be widened.
        TTagged tag t -> if tag == UInt8 24 -- cbor-in-cbor
                         then TTagged <$> ttag tag <*> pure t
                         else error "Unexpected TTagged"

        -- Terms that are unexpected.
        TFloat32 f  -> if not $ isNaN f
                       then pure $ TFloat32 f
                       else error "Unexpected 32bit representation of NaN"
        TFloat64 f  -> if not $ isNaN f
                       then pure $ TFloat64 f
                       else error "Unexpected 64bit representation of NaN"
        TMapI _     -> error "Unexpected TMapI"

        -- Representation of length can be widened.
        TArray len terms -> TArray <$> tlength len <*> mapM go terms

        -- Do not change anything, just go deeper.
        TArrayI terms    -> TArrayI <$> mapM go terms

        -- All other terms remain unchanged.
        t -> pure t

----------------------------------------

-- | Count the number of elements in a Term that can potentially be changed to
-- non-canonical representation.
countCanonicalBits :: Term -> Int
countCanonicalBits =
    (`execState` 0) .
        traverseCanonicalBits
            (\n -> add >> pure (TUInt n))
            (\n -> add >> pure (TNInt n))
            (add >> pure 0)
            dummyAdd
            dummyAdd
            dummyAdd
            dummyAdd
  where
    add :: State Int ()
    add = S.modify' (+1)

    dummyAdd :: t -> State Int t
    dummyAdd = (<$ add)

-- | Take a Term and randomly change it so that it's no longer canonical. The
-- number of introduced changes depends on the QuickCheck size parameter.
perturbCanonicity :: Term -> Gen Term
perturbCanonicity term = sized $ \sz -> do
    n <- choose (1, sz)
    toChange <- shuffle . take (countCanonicalBits term) $
        replicate n True ++ repeat False
    evalStateT (traverseCanonicalBits tuint tnint tzeroes tlength
                                      ttag tmapset tnan16 term)
               toChange
  where
      tuint n = shouldBeChanged >>= \case
          False -> pure $ TUInt n
          True  -> lift $ changeUInt TUInt identity n

      tnint n = shouldBeChanged >>= \case
          False -> pure $ TNInt n
          True  -> lift $ changeUInt TNInt negate n

      tzeroes = shouldBeChanged >>= \case
          False -> pure 0
          True  -> lift . sized $ \sz ->
              fromIntegral <$> choose (1, sz)

      tlength len = shouldBeChanged >>= \case
          False -> pure             len
          True  -> lift $ widenUInt len

      ttag n = shouldBeChanged >>= \case
          False -> pure             n
          True  -> lift $ widenUInt n

      tmapset terms = shouldBeChanged >>= \case
          False -> pure terms
          True -> lift $ oneof
              [ shuffle terms
              , addDuplicate terms
              ]
        where
            addDuplicate :: [t] -> Gen [t]
            addDuplicate ts = f ts <$> choose (0, length ts - 1)
              where
                f :: [t] -> Int -> [t]
                f [] _ = []
                f (x:xs) k
                    | k < 0     = x     : f xs  k
                    | k > 0     = x     : f xs (k - 1)
                    | otherwise = x : x : f xs (k - 1)

      tnan16 f = shouldBeChanged >>= \case
          False -> pure f
          True  -> lift (elements nans)

      widenUInt :: UInt -> Gen UInt
      widenUInt = \case
        UIntSmall w -> elements
            [ UInt8  $ fromIntegral w
            , UInt16 $ fromIntegral w
            , UInt32 $ fromIntegral w
            , UInt64 $ fromIntegral w
            ]
        UInt8 w -> elements
            [ UInt16 $ fromIntegral w
            , UInt32 $ fromIntegral w
            , UInt64 $ fromIntegral w
            ]
        UInt16 w -> elements
            [ UInt32 $ fromIntegral w
            , UInt64 $ fromIntegral w
            ]
        UInt32 w -> pure . UInt64 $ fromIntegral w
        UInt64 w -> pure $ UInt64 w

      changeUInt :: (UInt -> Term) -> (Integer -> Integer) -> UInt -> Gen Term
      changeUInt tint f (UIntSmall w) = elements
          [ tint . UInt8  $ fromIntegral w
          , tint . UInt16 $ fromIntegral w
          , tint . UInt32 $ fromIntegral w
          , tint . UInt64 $ fromIntegral w
          , mkTBigInt 0 . f $ fromIntegral w
          ]
      changeUInt tint f (UInt8 w) = elements
          [ tint . UInt16 $ fromIntegral w
          , tint . UInt32 $ fromIntegral w
          , tint . UInt64 $ fromIntegral w
          , mkTBigInt 0 . f $ fromIntegral w
          ]
      changeUInt tint f (UInt16 w) = elements
          [ tint . UInt32 $ fromIntegral w
          , tint . UInt64 $ fromIntegral w
          , mkTBigInt 0 . f $ fromIntegral w
          ]
      changeUInt tint f (UInt32 w) = elements
          [ tint . UInt64 $ fromIntegral w
          , mkTBigInt 0 . f $ fromIntegral w
          ]
      changeUInt _ f (UInt64 w) = pure . mkTBigInt 0 . f $ fromIntegral w

      mkTBigInt :: Word8 -> Integer -> Term
      mkTBigInt zs n =
          let bs = integerToBinaryRep zs n
              len = toUInt . fromIntegral $ length bs
          in TBigInt bs len n

      shouldBeChanged :: StateT [Bool] Gen Bool
      shouldBeChanged = do
          (x, xs) <- S.get >>= \case
              (x:xs) -> return (x, xs)
              [] ->
                  -- We use traverseCanonicalBits for both counting canonical bits and
                  -- changing them, hence the list will have just the right amount of
                  -- elements.
                  error "shouldBeChanged: impossible - not enough elements"
          S.put xs
          return x

-- All possible representations of a 16-bit NaN.
nans :: [Half]
nans = filter isNaN [Half w | w <- [minBound..maxBound]]
