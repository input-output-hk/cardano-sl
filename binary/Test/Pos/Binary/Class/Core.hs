module Test.Pos.Binary.Class.Core
    (
    -- * Properties
      encodedSizeProp
    , encodedSizeProp'
    , encodedListSizeProp
    , encodedSizePropGE
    , encodedSizePropGE'
    , encodedListSizePropGE
    -- * Generators
    , charGen
    , word16Gen
    , word32Gen
    , word64Gen
    , int32Gen
    , int64Gen
    , integerGen
    , floatGen
    -- * Utils
    , greaterEqual
    ) where

import           Data.Bits (unsafeShiftR)
import           Data.Char (GeneralCategory(Surrogate), generalCategory)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, forAll, listOf, suchThat, oneof, (===))
import           Universum

import           Pos.Binary.Class (Bi (..), serialize)

encodedSizeProp :: forall a. (Bi a, Show a) => Gen a -> Property
encodedSizeProp gen = forAll gen encodedSizeProp'

encodedSizeProp' :: forall a. (Bi a, Show a) => a -> Property
encodedSizeProp' a =
    encodedSize a === fromIntegral (length $ serialize a)

encodedListSizeProp :: forall a. (Bi a, Show a) => Gen a -> Property
encodedListSizeProp gen = forAll (listOf gen) $ \as ->
    encodedListSize as === fromIntegral (length $ serialize as)

-- Like @'==='@ but for @'>='@ rather than @'=='@.
greaterEqual :: (Ord a, Show a) => a -> a -> Property
greaterEqual a b =
    counterexample
        (show a ++ " ≱ " ++ show b)
        (a >= b)

encodedSizePropGE :: forall a. (Bi a, Show a) => Gen a -> Property
encodedSizePropGE gen = forAll gen encodedSizePropGE'

-- | The property that @encodedSize@ is not less than the actual size of
-- @'serizlize'@d term.  We only can guarantee inequality, since we are making
-- some estimates:
--  * when computing `crc32`, to avoid calculating the checksum, we put @4@
--    bytes.
encodedSizePropGE' :: forall a. (Bi a, Show a) => a -> Property
encodedSizePropGE' a =
    encodedSize a `greaterEqual` fromIntegral (length $ serialize a)

encodedListSizePropGE :: forall a. (Bi a, Show a) => Gen a -> Property
encodedListSizePropGE gen = forAll (listOf gen) $ \as ->
    encodedListSize as `greaterEqual` fromIntegral (length $ serialize as)

charGen :: Gen Char
charGen = oneof
    [ choose (chr 0, chr 0x7f)
    , choose (chr 0x80, chr 0x7ff)
    , choose (chr 0x800, chr 0xffff)
    , choose (chr 0x1000, chr 0x10ffff)
    ]
    `suchThat` (not . isSurrogate)
    where
    isSurrogate c = generalCategory c == Surrogate

word16Gen :: Gen Word16
word16Gen = oneof
    [ choose (0, 0x17)
    , choose (0x18, 0xff)
    , choose (0x100, 0xffff)
    ]

word32Gen :: Gen Word32
word32Gen = oneof
    [ choose (0, 0x17)
    , choose (0x18, 0xff)
    , choose (0x100, 0xffff)
    , choose (0x10000, 0xffffffff)
    ]

word64Gen :: Gen Word64
word64Gen = oneof
    [ choose (0, 0x17)
    , choose (0x18, 0xff)
    , choose (0x100, 0xffff)
    , choose (0x10000, 0xffffffff)
    , choose (0x100000000, 0xfffffffff)
    ]

int32Gen :: Gen Int32
int32Gen = do
    sign <- arbitrary
    -- `suchThat` is not needed, since anyway `word32` generates integers
    -- smaller than the limit.
    w <- word32Gen `suchThat` (<= unsafeShiftR (maxBound @Word32) 1)
    if sign
        then return $ fromIntegral w
        else return $ (-1) * fromIntegral w

int64Gen :: Gen Int64
int64Gen = do
    sign <- arbitrary
    -- `suchThat` is not needed, since anyway `word64` generates integers
    -- smaller than the limit.
    w <- word64Gen `suchThat` (<= unsafeShiftR (maxBound @Word64) 1)
    if sign
        then return $ fromIntegral w
        else return $ (-1) * fromIntegral w

integerGen :: Gen Integer
integerGen = oneof
    [ choose (min32, max32)
    , choose (min64, min32)
    , choose (max32, max64)
    , choose (8 * min64, min64)
    , choose (max64, 8 * max64)
    ]
    where
    min32 = fromIntegral $ minBound @Int32
    max32 = fromIntegral $ maxBound @Int32
    min64 = fromIntegral $ minBound @Int64
    max64 = fromIntegral $ maxBound @Int64

floatGen :: Gen Float
floatGen = oneof
    [ choose (-100.0, 100.0)
    , choose (-10.0**9, -10.0**8)
    , choose (10.0**8, 10.0**9)
    ]
