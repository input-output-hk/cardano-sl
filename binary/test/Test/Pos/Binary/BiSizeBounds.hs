{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Pos.Binary.BiSizeBounds
    ( tests
    ) where

import           Formatting (bprint, build)
import           Pos.Binary.Class
import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (unpack)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Typeable (TypeRep, typeRep)
import           Hedgehog (Gen, Group (..), Property, annotate, failure,
                           forAllWith, property, success)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Serokell.Data.Memory.Units (Byte)

bshow :: Buildable a => a -> String
bshow = unpack . toLazyText . bprint build

testSizeBounds :: (Buildable a, Bi a) => Gen a -> Property
testSizeBounds = testSizeBoundsWithBuilder bshow

testListSizeBounds :: (Buildable [a], Bi [a]) => Gen a -> Property
testListSizeBounds = testSizeBounds . Gen.list (Range.linear 0 300)

testSizeBoundsWithBuilder :: Bi a => (a -> String) -> Gen a -> Property
testSizeBoundsWithBuilder debug gen = property $ do
    x <- forAllWith debug gen
    case szVerify x of
        WithinBounds -> success
        BoundsAreSymbolic bounds -> do
            annotate ("Bounds are symbolic: " <> bshow bounds)
            failure
        OutOfBounds sz bounds -> do
            annotate ("Computed bounds: " <> bshow bounds)
            annotate ("Actual size:     " <> show sz)
            annotate ("Value: " <> debug x)
            failure

testSizeBoundsWithLength :: forall a. (Buildable a, Bi a) => (a -> Int) -> Gen a -> Property
testSizeBoundsWithLength lengthOf gen = property $ do
    x <- forAllWith bshow gen
    let ctx = M.fromList [ (typeRep (Proxy @(LengthOf a)),
                            SizeConstant $ fromIntegral (lengthOf x)) ]
    case szVerifyCtx ctx x of
        WithinBounds -> success
        BoundsAreSymbolic bounds -> do
            annotate ("Bounds are symbolic: " <> bshow bounds)
            failure
        OutOfBounds sz bounds -> do
            annotate ("Computed bounds: " <> bshow bounds)
            annotate ("Actual size:     " <> show sz)
            annotate ("Value: " <> bshow x)
            failure

encodedSize :: Bi a => a -> Byte
encodedSize = fromIntegral . BSL.length . toLazyByteString . encode

data ComparisonResult
    = WithinBounds
    | BoundsAreSymbolic Size
    | OutOfBounds Byte (Range Byte)

{-
instance Show ComparisonResult where
    show = \case
        WithinBounds         -> "WithinBounds"
        BoundsAreSymbolic sz -> "BoundsAreSymbolic " <> bshow sz
        OutOfBounds sz range -> "OutOfBounds " <> show sz <> " " <> bshow range
-}

szVerify :: Bi a => a -> ComparisonResult
szVerify = szVerifyCtx (M.fromList [])

szVerifyCtx :: Bi a => Map TypeRep SizeOverride -> a -> ComparisonResult
szVerifyCtx ctx x = case szSimplify (szWithCtx ctx (pure x)) of
    Left bounds -> BoundsAreSymbolic bounds
    Right range | lo range <= sz && sz <= hi range -> WithinBounds
    Right range | otherwise -> OutOfBounds sz range
  where
    sz = encodedSize x

tests :: IO Bool
tests =
    H.checkParallel $ Group "Encoded size bounds for core types."
    $ [ ("()"     , testSizeBoundsWithBuilder (const "()") (pure ()))
      , ("Bool"   , testSizeBounds Gen.bool)
      , ("Char"   , testSizeBounds Gen.unicode)
      , ("Char 2" , testSizeBounds Gen.latin1)
      , ("String" , testListSizeBounds Gen.unicode)
      , ("String 2" , testListSizeBounds Gen.latin1)
      , ("Word"   , testSizeBounds (Gen.word   Range.exponentialBounded))
      , ("Word8"  , testSizeBounds (Gen.word8  Range.exponentialBounded))
      , ("Word16" , testSizeBounds (Gen.word16 Range.exponentialBounded))
      , ("Word32" , testSizeBounds (Gen.word32 Range.exponentialBounded))
      , ("Word64" , testSizeBounds (Gen.word64 Range.exponentialBounded))
      , ("Int"    , testSizeBounds (Gen.int    Range.exponentialBounded))
      , ("Float"  , testSizeBounds (Gen.float  (Range.exponentialFloat 0 100000)))
      , ("Int32"  , testSizeBounds (Gen.int32  Range.exponentialBounded))
      , ("Int64"  , testSizeBounds (Gen.int64  Range.exponentialBounded))
      -- , ("Tagged _ Char", testSizeBounds (Tagged <$> Gen.unicode))
      , ("(Char, Bool)", testSizeBoundsWithBuilder
                           (\(a,b) -> "(" <> bshow a <> "," <> bshow b <> ")")
                           ((,) <$> Gen.unicode <*> Gen.bool))
      , ("(Char, Char, Bool)", testSizeBoundsWithBuilder
                               (\(a,b,c) -> "(" <> bshow a <> "," <> bshow b <> "," <> bshow c <> ")")
                               ((,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool))
      , ("(Char, Char, Bool, Bool)", testSizeBoundsWithBuilder
                               (\(a,b,c,d) -> "(" <> bshow a <> "," <> bshow b <> "," <> bshow c <> "," <> bshow d <> ")")
                               ((,,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool <*> Gen.bool))
      , ("ByteString"     , testSizeBoundsWithBuilder (show . BS.unpack)  (Gen.bytes (Range.linear 0 1000)))
      , ("Lazy.ByteString", testSizeBoundsWithBuilder (show . BSL.unpack) (BSL.fromStrict <$> Gen.bytes (Range.linear 0 1000)))
      , ("Text"           , testSizeBoundsWithLength length (Gen.text (Range.linear 0 1000) Gen.latin1))
      , ("Text 2"         , testSizeBoundsWithLength length (Gen.text (Range.linear 0 1000) Gen.unicode))
      --, ("[Word16]"       , testListSizeBounds (Gen.word16 Range.exponentialBounded))
      --, ("NonEmpty Word16"       , testListSizeBounds (Gen.word16 Range.exponentialBounded))
      --, ("Either Char Bool"  , testSizeBoundsWithBuilder (either bshow bshow) (Left <$> Gen.unicode))
      --, ("Either Char Bool 2", testSizeBoundsWithBuilder (either bshow bshow) (Right <$> Gen.bool))
      , ("Maybe Char"     , testSizeBounds (Gen.maybe Gen.unicode))
      , ("Maybe Char 2"   , testSizeBounds (Gen.maybe Gen.latin1))
      ]

