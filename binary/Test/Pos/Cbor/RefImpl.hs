{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reference implementation of CBOR (de)serialization.

module Test.Pos.Cbor.RefImpl
    ( serialise
    , deserialise
    , UInt(..)
    , Term(..)
    , toUInt
    , leadingZeroes
    , integerToBinaryRep
    , canonicalNaN
    -- * Properties
    , prop_InitialByte
    , prop_AdditionalInfo
    , prop_TokenHeader
    , prop_TokenHeader2
    , prop_Token
    , prop_Term
    -- * Properties of internal helpers
    , prop_integerToFromBytes
    , prop_word16ToFromNet
    , prop_word32ToFromNet
    , prop_word64ToFromNet
    , prop_halfToFromFloat
    ) where

import           Universum

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (span)
import qualified Data.Text.Encoding as T
import           Foreign (Storable (..), alloca, castPtr)
import           GHC.Float (RealFloat (..))
import           Numeric.Half (Half (..))
import qualified Numeric.Half as Half
import           System.IO.Unsafe (unsafeDupablePerformIO)

import           Test.QuickCheck.Arbitrary (Arbitrary (..), arbitraryBoundedIntegral)
import           Test.QuickCheck.Gen (Gen, choose, elements, frequency, oneof, resize, sized,
                                      suchThat, vectorOf)
import           Test.QuickCheck.Property (Property, conjoin, property, (.&&.), (===))

serialise :: Term -> LBS.ByteString
serialise = LBS.pack . encodeTerm

deserialise :: LBS.ByteString -> Term
deserialise bytes =
    case runDecoder decodeTerm (LBS.unpack bytes) of
      Just (term, []) -> term
      Just _          -> error "ReferenceImpl.deserialise: trailing data"
      Nothing         -> error "ReferenceImpl.deserialise: decoding failed"

------------------------------------------------------------------------

newtype Decoder a = Decoder { runDecoder :: [Word8] -> Maybe (a, [Word8]) }

decodeErr :: Decoder a
decodeErr = Decoder (const Nothing)

instance Functor Decoder where
    fmap f (Decoder d) = Decoder $ \x -> (first f) <$> d x

-- implemented using monad, yes
instance Applicative Decoder where
    pure  = return
    (<*>) = ap

instance Monad Decoder where
    return x = Decoder (\ws -> Just (x, ws))
    d >>= f  = Decoder (\ws -> case runDecoder d ws of
                                 Nothing       -> Nothing
                                 Just (x, ws') -> runDecoder (f x) ws')

getByte :: Decoder Word8
getByte =
    Decoder $ \case
        w:ws' -> Just (w, ws')
        _     -> Nothing

getBytes :: Integral n => n -> Decoder [Word8]
getBytes n =
    Decoder $ \ws ->
      case genericSplitAt n ws of
        (ws', [])   | genericLength ws' == n -> Just (ws', [])
                    | otherwise              -> Nothing
        (ws', ws'')                          -> Just (ws', ws'')

type Encoder a = a -> [Word8]

-- The initial byte of each data item contains both information about
-- the major type (the high-order 3 bits, described in Section 2.1) and
-- additional information (the low-order 5 bits).

data MajorType = MajorType0 | MajorType1 | MajorType2 | MajorType3
               | MajorType4 | MajorType5 | MajorType6 | MajorType7
  deriving (Show, Eq, Ord, Enum)

instance Arbitrary MajorType where
    arbitrary = elements [MajorType0 .. MajorType7]

encodeInitialByte :: MajorType -> Word -> Word8
encodeInitialByte mt ai
    | ai < 2^(5 :: Int)
    = fromIntegral (fromIntegral (fromEnum mt) `shiftL` 5 .|. ai)

    | otherwise
    = error "encodeInitialByte: invalid additional info value"

decodeInitialByte :: Word8 -> (MajorType, Word)
decodeInitialByte ib = ( toEnum $ fromIntegral $ ib `shiftR` 5
                       , fromIntegral $ ib .&. 0x1f)

prop_InitialByte :: Property
prop_InitialByte =
    conjoin [ (uncurry encodeInitialByte . decodeInitialByte) w8 === w8
            | w8 <- [minBound..maxBound] ]

-- When the value of the
-- additional information is less than 24, it is directly used as a
-- small unsigned integer.  When it is 24 to 27, the additional bytes
-- for a variable-length integer immediately follow; the values 24 to 27
-- of the additional information specify that its length is a 1-, 2-,
-- 4-, or 8-byte unsigned integer, respectively.  Additional information
-- value 31 is used for indefinite-length items, described in
-- Section 2.2.  Additional information values 28 to 30 are reserved for
-- future expansion.
--
-- In all additional information values, the resulting integer is
-- interpreted depending on the major type.  It may represent the actual
-- data: for example, in integer types, the resulting integer is used
-- for the value itself.  It may instead supply length information: for
-- example, in byte strings it gives the length of the byte string data
-- that follows.

data UInt =
       UIntSmall Word
     | UInt8     Word8
     | UInt16    Word16
     | UInt32    Word32
     | UInt64    Word64
  deriving (Eq, Show)

data AdditionalInformation =
       AiValue    UInt
     | AiIndefLen
     | AiReserved Word
  deriving (Eq, Show)

instance Arbitrary UInt where
    arbitrary =
        sized $ \n ->
            oneof $ take (1 + n `div` 2)
              [ UIntSmall <$> choose (0, 23)
              , UInt8     <$> arbitraryBoundedIntegral
              , UInt16    <$> arbitraryBoundedIntegral
              , UInt32    <$> arbitraryBoundedIntegral
              , UInt64    <$> arbitraryBoundedIntegral
              ]

instance Arbitrary AdditionalInformation where
    arbitrary =
        frequency
            [ (7, AiValue <$> arbitrary)
            , (2, pure AiIndefLen)
            , (1, AiReserved <$> choose (28, 30))
            ]

decodeAdditionalInfo :: Word -> Decoder AdditionalInformation
decodeAdditionalInfo = dec
  where
    dec n
      | n < 24 = return (AiValue (UIntSmall n))
    dec 24     = do w <- getByte
                    return (AiValue (UInt8 w))
    dec 25     = do ~[w1,w0] <- getBytes (2 :: Int)
                    let w = word16FromNet w1 w0
                    return (AiValue (UInt16 w))
    dec 26     = do ~[w3,w2,w1,w0] <- getBytes (4 :: Int)
                    let w = word32FromNet w3 w2 w1 w0
                    return (AiValue (UInt32 w))
    dec 27     = do ~[w7,w6,w5,w4,w3,w2,w1,w0] <- getBytes (8 :: Int)
                    let w = word64FromNet w7 w6 w5 w4 w3 w2 w1 w0
                    return (AiValue (UInt64 w))
    dec 31     = return AiIndefLen
    dec n
      | n < 31 = return (AiReserved n)
    dec _      = decodeErr

encodeAdditionalInfo :: AdditionalInformation -> (Word, [Word8])
encodeAdditionalInfo = enc
  where
    enc (AiValue (UIntSmall n))
      | n < 24               = (n, [])
      | otherwise            = error "invalid UIntSmall value"
    enc (AiValue (UInt8  w)) = (24, [w])
    enc (AiValue (UInt16 w)) = (25, [w1, w0])
                               where (w1, w0) = word16ToNet w
    enc (AiValue (UInt32 w)) = (26, [w3, w2, w1, w0])
                               where (w3, w2, w1, w0) = word32ToNet w
    enc (AiValue (UInt64 w)) = (27, [w7, w6, w5, w4,
                                     w3, w2, w1, w0])
                               where (w7, w6, w5, w4,
                                      w3, w2, w1, w0) = word64ToNet w
    enc  AiIndefLen          = (31, [])
    enc (AiReserved n)
      | n >= 28 && n < 31    = (n,  [])
      | otherwise            = error "invalid AiReserved value"

prop_AdditionalInfo :: AdditionalInformation -> Property
prop_AdditionalInfo ai =
    let (w, ws) = encodeAdditionalInfo ai
        Just (ai', _) = runDecoder (decodeAdditionalInfo w) ws
     in ai === ai'


data TokenHeader =
    TokenHeader MajorType AdditionalInformation
    deriving (Show, Eq)

instance Arbitrary TokenHeader where
    arbitrary = TokenHeader <$> arbitrary <*> arbitrary

decodeTokenHeader :: Decoder TokenHeader
decodeTokenHeader = do
    b <- getByte
    let (mt, ai) = decodeInitialByte b
    ai' <- decodeAdditionalInfo ai
    return (TokenHeader mt ai')

encodeTokenHeader :: Encoder TokenHeader
encodeTokenHeader (TokenHeader mt ai) =
    let (w, ws) = encodeAdditionalInfo ai
     in encodeInitialByte mt w : ws

prop_TokenHeader :: TokenHeader -> Property
prop_TokenHeader header =
    let ws                = encodeTokenHeader header
        Just (header', _) = runDecoder decodeTokenHeader ws
     in header === header'

prop_TokenHeader2 :: Property
prop_TokenHeader2 =
    conjoin [ w8 : extraused === encoded
            | w8 <- [minBound..maxBound]
            , let extra = [1..8]
                  Just (header, unused) = runDecoder decodeTokenHeader (w8 : extra)
                  encoded   = encodeTokenHeader header
                  extraused = take (8 - length unused) extra
            ]

data Token =
     MT0_UnsignedInt UInt
   | MT1_NegativeInt UInt
   | MT2_ByteString  UInt [Word8]
   | MT2_ByteStringIndef
   | MT3_String      UInt [Word8]
   | MT3_StringIndef
   | MT4_ArrayLen    UInt
   | MT4_ArrayLenIndef
   | MT5_MapLen      UInt
   | MT5_MapLenIndef
   | MT6_Tag     UInt
   | MT7_Simple  Word8
   | MT7_Float16 Half
   | MT7_Float32 Float
   | MT7_Float64 Double
   | MT7_Break
  deriving (Show, Eq)

instance Arbitrary Token where
    arbitrary =
        oneof
          [ MT0_UnsignedInt <$> arbitrary
          , MT1_NegativeInt <$> arbitrary
          , do ws <- arbitrary
               MT2_ByteString <$> arbitraryLengthUInt ws <*> pure ws
          , pure MT2_ByteStringIndef
          , do cs <- arbitrary
               let ws = encodeUTF8 cs
               MT3_String <$> arbitraryLengthUInt ws <*> pure ws
          , pure MT3_StringIndef
          , MT4_ArrayLen <$> arbitrary
          , pure MT4_ArrayLenIndef
          , MT5_MapLen <$> arbitrary
          , pure MT5_MapLenIndef
          , MT6_Tag     <$> arbitrary
          , MT7_Simple  <$> arbitrary
          , MT7_Float16 . getFloatSpecials <$> arbitrary
          , MT7_Float32 . getFloatSpecials <$> arbitrary
          , MT7_Float64 . getFloatSpecials <$> arbitrary
          , pure MT7_Break
          ]
      where
        arbitraryLengthUInt xs =
            let n = length xs in
              elements $
                   [ UIntSmall (fromIntegral n) | n < 24  ]
                ++ [ UInt8     (fromIntegral n) | n < 256 ]
                ++ [ UInt16    (fromIntegral n) | n < 65536 ]
                ++ [ UInt32    (fromIntegral n)
                   , UInt64    (fromIntegral n) ]


decodeToken :: Decoder Token
decodeToken = do
    header <- decodeTokenHeader
    extra  <- getBytes (tokenExtraLen header)
    either (const decodeErr) return (packToken header extra)

tokenExtraLen :: TokenHeader -> Word64
tokenExtraLen (TokenHeader MajorType2 (AiValue n)) = fromUInt n  -- bytestrings
tokenExtraLen (TokenHeader MajorType3 (AiValue n)) = fromUInt n  -- unicode strings
tokenExtraLen _                                    = 0

packToken :: TokenHeader -> [Word8] -> Either String Token
packToken (TokenHeader mt ai) extra = case (mt, ai) of
    -- Major type 0:  an unsigned integer.  The 5-bit additional information
    -- is either the integer itself (for additional information values 0
    -- through 23) or the length of additional data.
    (MajorType0, AiValue n)             -> return (MT0_UnsignedInt n)

    -- Major type 1:  a negative integer.  The encoding follows the rules
    -- for unsigned integers (major type 0), except that the value is
    -- then -1 minus the encoded unsigned integer.
    (MajorType1, AiValue n)             -> return (MT1_NegativeInt n)

    -- Major type 2:  a byte string.  The string's length in bytes is
    -- represented following the rules for positive integers (major type 0).
    (MajorType2, AiValue n)             -> return (MT2_ByteString n extra)
    (MajorType2, AiIndefLen)            -> return MT2_ByteStringIndef

    -- Major type 3:  a text string, specifically a string of Unicode
    -- characters that is encoded as UTF-8 [RFC3629].  The format of this
    -- type is identical to that of byte strings (major type 2), that is,
    -- as with major type 2, the length gives the number of bytes.
    (MajorType3, AiValue n)             -> return (MT3_String n extra)
    (MajorType3, AiIndefLen)            -> return MT3_StringIndef

    -- Major type 4:  an array of data items. The array's length follows the
    -- rules for byte strings (major type 2), except that the length
    -- denotes the number of data items, not the length in bytes that the
    -- array takes up.
    (MajorType4, AiValue n)             -> return (MT4_ArrayLen n)
    (MajorType4, AiIndefLen)            -> return  MT4_ArrayLenIndef

    -- Major type 5:  a map of pairs of data items. A map is comprised of
    -- pairs of data items, each pair consisting of a key that is
    -- immediately followed by a value. The map's length follows the
    -- rules for byte strings (major type 2), except that the length
    -- denotes the number of pairs, not the length in bytes that the map
    -- takes up.
    (MajorType5, AiValue n)             -> return (MT5_MapLen n)
    (MajorType5, AiIndefLen)            -> return  MT5_MapLenIndef

    -- Major type 6:  optional semantic tagging of other major types.
    -- The initial bytes of the tag follow the rules for positive integers
    -- (major type 0).
    (MajorType6, AiValue n)             -> return (MT6_Tag n)

    -- Major type 7 is for two types of data: floating-point numbers and
    -- "simple values" that do not need any content.  Each value of the
    -- 5-bit additional information in the initial byte has its own separate
    -- meaning, as defined in Table 1.
    --   | 0..23       | Simple value (value 0..23)                       |
    --   | 24          | Simple value (value 32..255 in following byte)   |
    --   | 25          | IEEE 754 Half-Precision Float (16 bits follow)   |
    --   | 26          | IEEE 754 Single-Precision Float (32 bits follow) |
    --   | 27          | IEEE 754 Double-Precision Float (64 bits follow) |
    --   | 28-30       | (Unassigned)                                     |
    --   | 31          | "break" stop code for indefinite-length items    |
    (MajorType7, AiValue (UIntSmall w)) -> return (MT7_Simple (fromIntegral w))
    (MajorType7, AiValue (UInt8     w)) -> return (MT7_Simple (fromIntegral w))
    (MajorType7, AiValue (UInt16    w)) -> return (MT7_Float16 (wordToHalf w))
    (MajorType7, AiValue (UInt32    w)) -> return (MT7_Float32 (wordToFloat w))
    (MajorType7, AiValue (UInt64    w)) -> return (MT7_Float64 (wordToDouble w))
    (MajorType7, AiIndefLen)            -> return MT7_Break
    _                                   -> Left "invalid token header"


encodeToken :: Encoder Token
encodeToken tok =
    let (header, extra) = unpackToken tok
     in encodeTokenHeader header ++ extra


unpackToken :: Token -> (TokenHeader, [Word8])
unpackToken tok = (\(mt, ai, ws) -> (TokenHeader mt ai, ws)) $ case tok of
    (MT0_UnsignedInt n)    -> (MajorType0, AiValue n,  [])
    (MT1_NegativeInt n)    -> (MajorType1, AiValue n,  [])
    (MT2_ByteString  n ws) -> (MajorType2, AiValue n,  ws)
    MT2_ByteStringIndef    -> (MajorType2, AiIndefLen, [])
    (MT3_String      n ws) -> (MajorType3, AiValue n,  ws)
    MT3_StringIndef        -> (MajorType3, AiIndefLen, [])
    (MT4_ArrayLen    n)    -> (MajorType4, AiValue n,  [])
    MT4_ArrayLenIndef      -> (MajorType4, AiIndefLen, [])
    (MT5_MapLen      n)    -> (MajorType5, AiValue n,  [])
    MT5_MapLenIndef        -> (MajorType5, AiIndefLen, [])
    (MT6_Tag     n)        -> (MajorType6, AiValue n,  [])
    (MT7_Simple  n)
               | n <= 23   -> (MajorType7, AiValue (UIntSmall (fromIntegral n)), [])
               | otherwise -> (MajorType7, AiValue (UInt8     n), [])
    (MT7_Float16 f)        -> (MajorType7, AiValue (UInt16 (halfToWord f)),   [])
    (MT7_Float32 f)        -> (MajorType7, AiValue (UInt32 (floatToWord f)),  [])
    (MT7_Float64 f)        -> (MajorType7, AiValue (UInt64 (doubleToWord f)), [])
    MT7_Break              -> (MajorType7, AiIndefLen, [])


fromUInt :: UInt -> Word64
fromUInt (UIntSmall w) = fromIntegral w
fromUInt (UInt8     w) = fromIntegral w
fromUInt (UInt16    w) = fromIntegral w
fromUInt (UInt32    w) = fromIntegral w
fromUInt (UInt64    w) = fromIntegral w

toUInt :: Word64 -> UInt
toUInt n
  | n < 24                                 = UIntSmall (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word8)  = UInt8     (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word16) = UInt16    (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word32) = UInt32    (fromIntegral n)
  | otherwise                              = UInt64    n

lengthUInt :: [a] -> UInt
lengthUInt = toUInt . fromIntegral . length

decodeUTF8 :: [Word8] -> Either String [Char]
decodeUTF8 = either (Left . show) (return . toString) . T.decodeUtf8' . BS.pack

encodeUTF8 :: [Char] -> [Word8]
encodeUTF8 = BS.unpack . T.encodeUtf8 . toText

reservedSimple :: Word8 -> Bool
reservedSimple w = w >= 20 && w <= 31

reservedTag :: Word64 -> Bool
reservedTag w = w <= 5

prop_Token :: Token -> Property
prop_Token token =
    let ws = encodeToken token
        Just (token', []) = runDecoder decodeToken ws
     in token `eqToken` token'
  where
    -- NaNs are so annoying...
    eqToken :: Token -> Token -> Property
    eqToken (MT7_Float16 f) (MT7_Float16 f') | isNaN f && isNaN f' = property True
    eqToken (MT7_Float32 f) (MT7_Float32 f') | isNaN f && isNaN f' = property True
    eqToken (MT7_Float64 f) (MT7_Float64 f') | isNaN f && isNaN f' = property True
    eqToken a b                              = a === b

data Term = TUInt   UInt
          | TNInt   UInt
          | TBigInt [Word8] {- binary encoding of the Integer -}
                    UInt    {- representation of length in the binary encoding -}
                    Integer
          | TBytes  UInt {- representation of length -} [Word8]
          | TBytess  [[Word8]]
          | TString UInt {- representation of length -} [Char]
          | TStrings [[Char]]
          | TArray  UInt {- representation of length -} [Term]
          | TArrayI [Term]
          | TMap    UInt {- representation of length -} [(Term, Term)]
          | TMapI   [(Term, Term)]
          | TTagged UInt Term
          | TTrue
          | TFalse
          | TNull
          | TUndef
          | TSimple  Word8
          | TFloat16 Half
          | TFloat32 Float
          | TFloat64 Double
  deriving (Show, Eq)

-- | Encode an Integer as a CBOR-compliant bytestring given a number of leading
-- zeroes in its representation.
integerToBinaryRep :: Word8 -> Integer -> [Word8]
integerToBinaryRep zs n =
    replicate (fromIntegral zs) 0 ++ (integerToBytes $ if n >= 0
                                                       then n
                                                       else -1 - n)

instance Arbitrary Term where
    arbitrary =
        frequency
            [ (1, TUInt    <$> arbitrary)
            , (1, TNInt    <$> arbitrary)
            , (1, arbitrary >>= \(LargeInteger n) -> do
                      bs <- sized $ \sz -> integerToBinaryRep
                          <$> (fromIntegral <$> choose (0, sz))
                          <*> pure n
                      TBigInt <$> pure bs <*> arbitraryLengthRep bs <*> pure n)
            , (1, arbitrary >>= \bs -> TBytes <$> arbitraryLengthRep bs <*> pure bs)
            , (1, TBytess  <$> arbitrary)
            , (1, arbitrary >>= \bs ->
                      TString <$> arbitraryLengthRep (encodeUTF8 bs) <*> pure bs)
            , (1, TStrings <$> arbitrary)
            , (2, listOfSmaller arbitrary >>= \arr ->
                      TArray <$> arbitraryLengthRep arr <*> pure arr)
            , (2, TArrayI  <$> listOfSmaller arbitrary)
            , (2, listOfSmaller ((,) <$> arbitrary <*> arbitrary) >>= \arr ->
                      TMap <$> arbitraryLengthRep arr <*> pure arr)
            , (2, TMapI    <$> listOfSmaller ((,) <$> arbitrary <*> arbitrary))
            , (1, TTagged  <$> arbitraryTag <*> sized (\sz -> resize (max 0 (sz-1)) arbitrary))
            , (1, pure TFalse)
            , (1, pure TTrue)
            , (1, pure TNull)
            , (1, pure TUndef)
            , (1, TSimple  <$> arbitrary `suchThat` (not . reservedSimple))
            , (1, TFloat16 <$> arbitrary)
            , (1, TFloat32 <$> arbitrary)
            , (1, TFloat64 <$> arbitrary)
            ]
      where
        listOfSmaller :: Gen a -> Gen [a]
        listOfSmaller gen =
            sized $ \n -> do
                k <- choose (0,n)
                vectorOf k (resize (n `div` (k+1)) gen)

        arbitraryLengthRep :: [a] -> Gen UInt
        arbitraryLengthRep xs
          | len <= 23         = elements [ UIntSmall $ fromIntegral len
                                         , UInt8     $ fromIntegral len
                                         , UInt16    $ fromIntegral len
                                         , UInt32    $ fromIntegral len
                                         , UInt64    $ fromIntegral len
                                         ]
          | len <= 0xff       = elements [ UInt8  $ fromIntegral len
                                         , UInt16 $ fromIntegral len
                                         , UInt32 $ fromIntegral len
                                         , UInt64 $ fromIntegral len
                                         ]
          | len <= 0xffff     = elements [ UInt16 $ fromIntegral len
                                         , UInt32 $ fromIntegral len
                                         , UInt64 $ fromIntegral len
                                         ]
          | len <= 0xffffffff = elements [ UInt32 $ fromIntegral len
                                         , UInt64 $ fromIntegral len
                                         ]
          | otherwise         = pure     $ UInt64 $ fromIntegral len
          where
            len :: Word
            len = fromIntegral (length xs)

        arbitraryTag = arbitrary `suchThat` (not . reservedTag . fromUInt)

    shrink (TUInt   n)    = [ TUInt    n'   | n' <- shrink n ]
    shrink (TNInt   n)    = [ TNInt    n'   | n' <- shrink n ]
    shrink (TBigInt bs len n) = [ let bs' = integerToBinaryRep zs n'
                                  in TBigInt bs' (replaceLen bs' len) n'
                                | (zs, n') <- shrink (leadingZeroes bs, n) ]
    shrink (TBytes n ws)  = [ TBytes (replaceLen ws' n) ws' | ws' <- shrink ws ]
    shrink (TBytess wss)  = [ TBytess  wss' | wss' <- shrink wss ]
    shrink (TString n ws) = [ TString (replaceLen (encodeUTF8 ws') n) ws'
                            | ws' <- shrink ws ]
    shrink (TStrings wss) = [ TStrings wss' | wss' <- shrink wss ]

    shrink (TArray n xs@[x]) = x : [ TArray (replaceLen xs' n) xs' | xs' <- shrink xs ]
    shrink (TArray n xs)     =     [ TArray (replaceLen xs' n) xs' | xs' <- shrink xs ]
    shrink (TArrayI xs@[x]) = x : [ TArrayI xs' | xs' <- shrink xs ]
    shrink (TArrayI xs)     =     [ TArrayI xs' | xs' <- shrink xs ]

    shrink (TMap n xys@[(x,y)]) = x : y : [ TMap (replaceLen xys' n) xys'
                                          | xys' <- shrink xys ]
    shrink (TMap n xys)         =         [ TMap (replaceLen xys' n) xys'
                                          | xys' <- shrink xys ]
    shrink (TMapI xys@[(x,y)]) = x : y : [ TMapI xys' | xys' <- shrink xys ]
    shrink (TMapI xys)         =         [ TMapI xys' | xys' <- shrink xys ]

    shrink (TTagged w t) = [ TTagged w' t' | (w', t') <- shrink (w, t)
                                           , not (reservedTag (fromUInt w')) ]

    shrink TFalse = []
    shrink TTrue  = []
    shrink TNull  = []
    shrink TUndef = []

    shrink (TSimple  w) = [ TSimple  w' | w' <- shrink w, not (reservedSimple w) ]
    shrink (TFloat16 f) = [ TFloat16 f' | f' <- shrink f ]
    shrink (TFloat32 f) = [ TFloat32 f' | f' <- shrink f ]
    shrink (TFloat64 f) = [ TFloat64 f' | f' <- shrink f ]

-- Shrinking produces smaller values, so there will be no overflow.
replaceLen :: [a] -> UInt -> UInt
replaceLen xs = \case
    UIntSmall _ -> UIntSmall $ fromIntegral len
    UInt8     _ -> UInt8     $ fromIntegral len
    UInt16    _ -> UInt16    $ fromIntegral len
    UInt32    _ -> UInt32    $ fromIntegral len
    UInt64    _ -> UInt64    $ fromIntegral len
  where
    len = length xs

decodeTerm :: Decoder Term
decodeTerm = decodeToken >>= decodeTermFrom

decodeTermFrom :: Token -> Decoder Term
decodeTermFrom = \case
    MT0_UnsignedInt n   -> return (TUInt n)
    MT1_NegativeInt n   -> return (TNInt n)

    MT2_ByteString n bs -> return (TBytes n bs)
    MT2_ByteStringIndef -> decodeBytess []

    MT3_String n ws     -> either (const decodeErr) (return . TString n) $
                           decodeUTF8 ws
    MT3_StringIndef     -> decodeStrings []

    MT4_ArrayLen len    -> decodeArrayN len
    MT4_ArrayLenIndef   -> decodeArray []

    MT5_MapLen  len     -> decodeMapN len
    MT5_MapLenIndef     -> decodeMap  []

    MT6_Tag     tag     -> decodeTagged tag

    MT7_Simple  20      -> return TFalse
    MT7_Simple  21      -> return TTrue
    MT7_Simple  22      -> return TNull
    MT7_Simple  23      -> return TUndef
    MT7_Simple  w       -> return (TSimple w)
    MT7_Float16 f       -> return (TFloat16 f)
    MT7_Float32 f       -> return (TFloat32 f)
    MT7_Float64 f       -> return (TFloat64 f)
    MT7_Break           -> decodeErr


decodeBytess :: [[Word8]] -> Decoder Term
decodeBytess acc =
    decodeToken >>= \case
        MT7_Break           -> return $! TBytess (reverse acc)
        MT2_ByteString _ bs -> decodeBytess (bs : acc)
        _                   -> decodeErr

decodeStrings :: [String] -> Decoder Term
decodeStrings acc =
    decodeToken >>= \case
        MT7_Break        -> return $! TStrings (reverse acc)
        MT3_String _ ws  -> do cs <- either (const decodeErr) return (decodeUTF8 ws)
                               decodeStrings (cs : acc)
        _                -> decodeErr

decodeArrayN :: UInt -> Decoder Term
decodeArrayN n = TArray n <$!> replicateM (fromIntegral $ fromUInt n) decodeTerm

decodeArray :: [Term] -> Decoder Term
decodeArray acc = do
    tk <- decodeToken
    case tk of
      MT7_Break -> return $! TArrayI (reverse acc)
      _         -> do
        tm <- decodeTermFrom tk
        decodeArray (tm : acc)

decodeMapN :: UInt -> Decoder Term
decodeMapN n = TMap n
    <$!> replicateM (fromIntegral $ fromUInt n) ((,) <$> decodeTerm <*> decodeTerm)

decodeMap :: [(Term, Term)] -> Decoder Term
decodeMap acc = do
    tk <- decodeToken
    case tk of
      MT7_Break -> return $! TMapI (reverse acc)
      _         -> do
        tm  <- decodeTermFrom tk
        tm' <- decodeTerm
        decodeMap ((tm, tm') : acc)

decodeTagged :: UInt -> Decoder Term
decodeTagged tag = case fromUInt tag of
    2 -> do
        (bs, len) <- decodeToken >>= \case
            MT2_ByteString len bs -> return (bs, len)
            _ -> decodeErr
        let !n = integerFromBytes bs
        return $ TBigInt bs len n
    3 -> do
        (bs, len) <- decodeToken >>= \case
            MT2_ByteString len bs -> return (bs, len)
            _ -> decodeErr
        let !n = integerFromBytes bs
        return $ TBigInt bs len (-1 - n)
    _ -> do
        tm <- decodeTerm
        return (TTagged tag tm)

-- | Get a number of leading zeroes in the binary representation of an
-- Integer. Note that if all of the bytes are zeroes (i.e. we're dealing with
-- the representation of 0), we don't count the last one.
leadingZeroes :: [Word8] -> Word8
leadingZeroes bs = fromIntegral $ case span (== 0) bs of
    ([], []) -> error "leadingZeroes: unexpected empty list"
    (zs, []) -> length zs - 1
    (zs, _)  -> length zs

integerFromBytes :: [Word8] -> Integer
integerFromBytes []       = 0
integerFromBytes (w0:ws0) = go (fromIntegral w0) ws0
  where
    go !acc []     = acc
    go !acc (w:ws) = go (acc `shiftL` 8 + fromIntegral w) ws

integerToBytes :: Integer -> [Word8]
integerToBytes n0
    | n0 == 0   = [0]
    | n0 < 0    = reverse (go (-n0))
    | otherwise = reverse (go n0)
  where
    go n | n == 0    = []
         | otherwise = narrow n : go (n `shiftR` 8)

    narrow :: Integer -> Word8
    narrow = fromIntegral

prop_integerToFromBytes :: LargeInteger -> Property
prop_integerToFromBytes (LargeInteger n)
  | n >= 0 =
    let ws = integerToBytes n
        n' = integerFromBytes ws
     in n === n'
  | otherwise =
    let ws = integerToBytes n
        n' = integerFromBytes ws
     in n === -n'

-------------------------------------------------------------------------------

encodeTerm :: Encoder Term
encodeTerm (TUInt n)       = encodeToken (MT0_UnsignedInt n)
encodeTerm (TNInt n)       = encodeToken (MT1_NegativeInt n)
encodeTerm (TBigInt ws len n)
               | n >= 0    = encodeToken (MT6_Tag (UIntSmall 2))
                          <> encodeToken (MT2_ByteString len ws)
               | otherwise = encodeToken (MT6_Tag (UIntSmall 3))
                          <> encodeToken (MT2_ByteString len ws)
encodeTerm (TBytes len ws) = encodeToken (MT2_ByteString len ws)
encodeTerm (TBytess wss)   = encodeToken MT2_ByteStringIndef
                          <> mconcat [ encodeToken (MT2_ByteString len ws)
                                     | ws <- wss
                                     , let len = lengthUInt ws ]
                          <> encodeToken MT7_Break
encodeTerm (TString len cs) = let ws  = encodeUTF8 cs in
                              encodeToken (MT3_String len ws)
encodeTerm (TStrings css)  = encodeToken MT3_StringIndef
                          <> mconcat [ encodeToken (MT3_String len ws)
                                     | cs <- css
                                     , let ws  = encodeUTF8 cs
                                           len = lengthUInt ws ]
                          <> encodeToken MT7_Break
encodeTerm (TArray len ts) = encodeToken (MT4_ArrayLen len)
                          <> mconcat (map encodeTerm ts)
encodeTerm (TArrayI ts)    = encodeToken MT4_ArrayLenIndef
                          <> mconcat (map encodeTerm ts)
                          <> encodeToken MT7_Break
encodeTerm (TMap len kvs)  = encodeToken (MT5_MapLen len)
                          <> mconcat [ encodeTerm k <> encodeTerm v
                                     | (k,v) <- kvs ]
encodeTerm (TMapI   kvs)   = encodeToken MT5_MapLenIndef
                          <> mconcat [ encodeTerm k <> encodeTerm v
                                     | (k,v) <- kvs ]
                          <> encodeToken MT7_Break
encodeTerm (TTagged tag t) = encodeToken (MT6_Tag tag)
                          <> encodeTerm t
encodeTerm  TFalse         = encodeToken (MT7_Simple 20)
encodeTerm  TTrue          = encodeToken (MT7_Simple 21)
encodeTerm  TNull          = encodeToken (MT7_Simple 22)
encodeTerm  TUndef         = encodeToken (MT7_Simple 23)
encodeTerm (TSimple  w)    = encodeToken (MT7_Simple w)
encodeTerm (TFloat16 f)    = encodeToken (MT7_Float16 f)
encodeTerm (TFloat32 f)    = encodeToken (MT7_Float32 f)
encodeTerm (TFloat64 f)    = encodeToken (MT7_Float64 f)


-------------------------------------------------------------------------------

prop_Term :: Term -> Property
prop_Term term =
    let ws = encodeTerm term
        Just (term', []) = runDecoder decodeTerm ws
     in term `eqTerm` term'
  where
    -- NaNs are so annoying...
    eqTerm :: Term -> Term -> Property
    eqTerm (TArray n ts) (TArray n' ts') = n == n' .&&. conjoin (zipWith eqTerm ts ts')
    eqTerm (TArrayI ts)  (TArrayI ts')   = conjoin (zipWith eqTerm ts ts')
    eqTerm (TMap n ts)   (TMap n' ts')   = n == n' .&&. conjoin (zipWith eqTermPair ts ts')
    eqTerm (TMapI   ts)  (TMapI   ts')   = conjoin (zipWith eqTermPair ts ts')
    eqTerm (TTagged w t) (TTagged w' t') = w === w' .&&. eqTerm t t'
    eqTerm (TFloat16 f)  (TFloat16 f')   | isNaN f && isNaN f' = property True
    eqTerm (TFloat32 f)  (TFloat32 f')   | isNaN f && isNaN f' = property True
    eqTerm (TFloat64 f)  (TFloat64 f')   | isNaN f && isNaN f' = property True
    eqTerm a b                           = a === b

    eqTermPair :: (Term, Term) -> (Term, Term) -> Property
    eqTermPair (a, b) (a', b') = eqTerm a a' .&&. eqTerm b b'

-- | Canonical representation of a NaN, as per
-- https://tools.ietf.org/html/rfc7049#section-3.9.
canonicalNaN :: Half
canonicalNaN = Half 0x7e00

-------------------------------------------------------------------------------

word16FromNet :: Word8 -> Word8 -> Word16
word16FromNet w1 w0 =
      fromIntegral w1 `shiftL` 8 -- 8*1
  .|. fromIntegral w0 `shiftL` 0 -- 8*0

word16ToNet :: Word16 -> (Word8, Word8)
word16ToNet w =
    ( fromIntegral ((w `shiftR` 8 {- 8*1 -}) .&. 0xff)
    , fromIntegral ((w `shiftR` 0 {- 8*0 -}) .&. 0xff)
    )

word32FromNet :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word32FromNet w3 w2 w1 w0 =
      fromIntegral w3 `shiftL` (8*3)
  .|. fromIntegral w2 `shiftL` (8*2)
  .|. fromIntegral w1 `shiftL` 8 -- 8*1
  .|. fromIntegral w0 `shiftL` 0 -- 8*0

word32ToNet :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToNet w =
    ( fromIntegral ((w `shiftR` (8*3)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*2)) .&. 0xff)
    , fromIntegral ((w `shiftR` 8 {- 8*1 -}) .&. 0xff)
    , fromIntegral ((w `shiftR` 0 {- 8*0 -}) .&. 0xff)
    )

word64FromNet :: Word8 -> Word8 -> Word8 -> Word8 ->
                 Word8 -> Word8 -> Word8 -> Word8 -> Word64
word64FromNet w7 w6 w5 w4 w3 w2 w1 w0 =
      fromIntegral w7 `shiftL` (8*7)
  .|. fromIntegral w6 `shiftL` (8*6)
  .|. fromIntegral w5 `shiftL` (8*5)
  .|. fromIntegral w4 `shiftL` (8*4)
  .|. fromIntegral w3 `shiftL` (8*3)
  .|. fromIntegral w2 `shiftL` (8*2)
  .|. fromIntegral w1 `shiftL` 8 -- 8*1
  .|. fromIntegral w0 `shiftL` 0 -- 8*0

word64ToNet :: Word64 -> (Word8, Word8, Word8, Word8,
                          Word8, Word8, Word8, Word8)
word64ToNet w =
    ( fromIntegral ((w `shiftR` (8*7)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*6)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*5)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*4)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*3)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*2)) .&. 0xff)
    , fromIntegral ((w `shiftR` 8 {- 8*1 -}) .&. 0xff)
    , fromIntegral ((w `shiftR` 0 {- 8*0 -}) .&. 0xff)
    )

prop_word16ToFromNet :: Word8 -> Word8 -> Property
prop_word16ToFromNet w1 w0 =
    word16ToNet (word16FromNet w1 w0) === (w1, w0)

prop_word32ToFromNet :: Word8 -> Word8 -> Word8 -> Word8 -> Property
prop_word32ToFromNet w3 w2 w1 w0 =
    word32ToNet (word32FromNet w3 w2 w1 w0) === (w3, w2, w1, w0)

prop_word64ToFromNet :: Word8 -> Word8 -> Word8 -> Word8 ->
                        Word8 -> Word8 -> Word8 -> Word8 -> Property
prop_word64ToFromNet w7 w6 w5 w4 w3 w2 w1 w0 =
     word64ToNet (word64FromNet w7 w6 w5 w4 w3 w2 w1 w0)
 === (w7, w6, w5, w4, w3, w2, w1, w0)

wordToHalf :: Word16 -> Half
wordToHalf = Half.Half . fromIntegral

wordToFloat :: Word32 -> Float
wordToFloat = toFloat

wordToDouble :: Word64 -> Double
wordToDouble = toFloat

toFloat :: (Storable word, Storable float) => word -> float
toFloat w =
    unsafeDupablePerformIO $ alloca $ \buf -> do
      poke (castPtr buf) w
      peek buf

halfToWord :: Half -> Word16
halfToWord (Half.Half w) = fromIntegral w

floatToWord :: Float -> Word32
floatToWord = fromFloat

doubleToWord :: Double -> Word64
doubleToWord = fromFloat

fromFloat :: (Storable word, Storable float) => float -> word
fromFloat float =
    unsafeDupablePerformIO $ alloca $ \buf -> do
            poke (castPtr buf) float
            peek buf

-- Note: some NaNs do not roundtrip https://github.com/ekmett/half/issues/3
-- but all the others had better
prop_halfToFromFloat :: Property
prop_halfToFromFloat =
    conjoin . map roundTrip
            . filter (not . isNaN . Half.Half)
            $ [minBound..maxBound]
  where
    roundTrip w =
      w === (Half.getHalf . Half.toHalf . Half.fromHalf . Half.Half $ w)

instance Arbitrary Half where
    arbitrary = Half.Half . fromIntegral <$> (arbitrary :: Gen Word16)

newtype FloatSpecials n = FloatSpecials { getFloatSpecials :: n }
    deriving (Show, Eq)

instance (Arbitrary n, RealFloat n) => Arbitrary (FloatSpecials n) where
    arbitrary =
        frequency
            [ (7, FloatSpecials <$> arbitrary)
            , (1, pure (FloatSpecials (1/0)) )  -- +Infinity
            , (1, pure (FloatSpecials (0/0)) )  --  NaN
            , (1, pure (FloatSpecials (-1/0)) ) -- -Infinity
            ]

newtype LargeInteger = LargeInteger Integer
    deriving (Show, Eq)

instance Arbitrary LargeInteger where
    arbitrary =
        sized $ \n ->
            oneof $ take (1 + n `div` 10)
              [ LargeInteger .          fromIntegral <$> (arbitrary :: Gen Int8)
              , LargeInteger .          fromIntegral <$> choose (minBound, maxBound :: Int64)
              , LargeInteger . bigger . fromIntegral <$> choose (minBound, maxBound :: Int64)
              ]
      where
        bigger n = n * abs n
