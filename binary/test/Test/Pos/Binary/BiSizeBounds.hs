{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Data.Tagged (Tagged (..))
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

-- | Configuration for a single test case.
data TestConfig a = TestConfig
    { debug    :: a -> String      -- ^ Pretty-print values
    , lengthOf :: Maybe (a -> Int) -- ^ Compute the length of a value
    , lengthTy :: TypeRep         -- ^ TypeRep used to represent the length.
    , gen      :: Gen a           -- ^ Generator
    , precise  :: Bool            -- ^ Must estimates be exact?
    }

-- | Default configuration, for @Buildable@ types.
cfg :: forall a. (Typeable a, Buildable a) => TestConfig a
cfg = TestConfig { debug    = bshow
                 , lengthOf = Nothing
                 , lengthTy = typeRep (Proxy @(LengthOf a))
                 , gen      = Gen.discard
                 , precise  = False
                 }

-- | Default configuration, for @Show@able types.
scfg :: forall a. (Typeable a, Show a) => TestConfig a
scfg = TestConfig { debug    = show
                  , lengthOf = Nothing
                  , lengthTy = typeRep (Proxy @(LengthOf a))
                  , gen      = Gen.discard
                  , precise  = False
                  }

runTest :: forall a. Bi a => TestConfig a -> Property
runTest TestConfig{..} = property $ do
    x <- forAllWith debug gen

    let ctx = case lengthOf of
            Nothing  -> []
            Just len -> [ (lengthTy, SizeConstant $ fromIntegral (len x)) ]

        badBounds sz bounds = do
            annotate ("Computed bounds: " <> bshow bounds)
            annotate ("Actual size:     " <> show sz)
            annotate ("Value: " <> debug x)
            failure

    case szVerify (M.fromList ctx) x of
        Exact -> success
        WithinBounds _ _  | not precise -> success
        WithinBounds sz bounds -> badBounds sz bounds
        BoundsAreSymbolic bounds -> do
            annotate ("Bounds are symbolic: " <> bshow bounds)
            failure
        OutOfBounds sz bounds -> badBounds sz bounds

-- | The possible results from @szVerify@, describing various ways
--   a size can or cannot be found within a certain range.
data ComparisonResult
    = Exact                          -- ^ Size matched the bounds, and the bounds were exact.
    | WithinBounds Byte (Range Byte) -- ^ Size matched the bounds, but the bounds are not exact.
    | BoundsAreSymbolic Size         -- ^ The bounds could not be reduced to a numerical range.
    | OutOfBounds Byte (Range Byte)  -- ^ The size fell outside of the bounds.

-- | For a given value @x :: a@ with @Bi a@, check that the encoded size
--   of @x@ falls within the statically-computed size range for @a@.
szVerify :: Bi a => Map TypeRep SizeOverride -> a -> ComparisonResult
szVerify ctx x = case szSimplify (szWithCtx ctx (pure x)) of
    Left bounds -> BoundsAreSymbolic bounds
    Right range | lo range <= sz && sz <= hi range ->
                      if lo range == hi range
                      then Exact
                      else WithinBounds sz range
    Right range | otherwise -> OutOfBounds sz range
  where
    sz = fromIntegral $ BSL.length $ toLazyByteString $ encode x

tests :: IO Bool
tests =
    let listOf = Gen.list (Range.linear 0 300)
        wordGen = Gen.word Range.exponentialBounded
    in H.checkParallel $ Group "Encoded size bounds for core types."
       $ [ ("()"     , runTest $ scfg { gen = pure (), precise = True })
         , ("Bool"   , runTest $ cfg { gen = Gen.bool, precise = True })
         , ("Char"   , runTest $ cfg { gen = Gen.unicode })
         , ("Char 2" , runTest $ cfg { gen = Gen.latin1 })
         , ("String"   , runTest $ cfg { gen = listOf Gen.unicode
                                              , lengthOf = Just length
                                              , lengthTy = typeRep (Proxy @(LengthOf [Char])) })
         , ("String 2" , runTest $ cfg { gen = listOf Gen.latin1
                                              , lengthOf = Just length
                                              , lengthTy = typeRep (Proxy @(LengthOf [Char])) })
         , ("Word"   , runTest $ cfg { gen = Gen.word   Range.exponentialBounded })
         , ("Word8"  , runTest $ cfg { gen = Gen.word8  Range.exponentialBounded })
         , ("Word16" , runTest $ cfg { gen = Gen.word16 Range.exponentialBounded })
         , ("Word32" , runTest $ cfg { gen = Gen.word32 Range.exponentialBounded })
         , ("Word64" , runTest $ cfg { gen = Gen.word64 Range.exponentialBounded })
         , ("Int"    , runTest $ cfg { gen = Gen.int    Range.exponentialBounded })
         , ("Float"  , runTest $ cfg { gen = Gen.float (Range.exponentialFloat (-1000000) 1000000) })
         , ("Int32"  , runTest $ cfg { gen = Gen.int32  Range.exponentialBounded })
         , ("Int64"  , runTest $ cfg { gen = Gen.int64  Range.exponentialBounded })
         , ("Tagged () Word32", runTest $ (scfg :: TestConfig (Tagged () Word32))
               { gen = Tagged <$> Gen.word32 Range.exponentialBounded })
         , ("(Char, Bool)",
               runTest $ scfg { gen = (,) <$> Gen.unicode <*> Gen.bool })
         , ("(Char, Char, Bool)",
               runTest $ scfg { gen = ((,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool) })
         , ("(Char, Char, Bool, Bool)",
               runTest $ scfg { gen = ((,,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool <*> Gen.bool) })
         , ("ByteString"     , runTest $ (scfg :: TestConfig BS.ByteString)
               { debug = show . (BS.unpack :: BS.ByteString -> [Word8])
               , lengthOf = Just (fromIntegral . BS.length)
               , gen = Gen.bytes (Range.linear 0 1000) })
         , ("Lazy.ByteString", runTest $ (scfg :: TestConfig BSL.ByteString)
               { debug = show . (BSL.unpack :: BSL.ByteString -> [Word8])
               , lengthOf = Just (fromIntegral . BSL.length)
               , gen = BSL.fromStrict <$> Gen.bytes (Range.linear 0 1000) })
         , ("Text", runTest $ cfg
               { lengthOf = Just length
               , lengthTy = typeRep (Proxy @(LengthOf [Char]))
               , gen = Gen.text (Range.linear 0 1000) Gen.latin1 })
         , ("Text 2", runTest $ cfg
               { lengthOf = Just length
               , lengthTy = typeRep (Proxy @(LengthOf [Char]))
               , gen = Gen.text (Range.linear 0 1000) Gen.unicode })
         , ("[Bool]"       , runTest $ scfg
               { gen = listOf Gen.bool
               , lengthOf = Just length
               , precise = True})
         , ("NonEmpty Bool", runTest $ scfg
               { gen = listOf Gen.bool
               , lengthOf = Just length
               , precise = True })
         , ("Either Bool Word", runTest $ (scfg :: TestConfig (Either Bool Word))
               { gen = Left  <$> Gen.bool })
         , ("Either Bool Word", runTest $ (scfg :: TestConfig (Either Bool Word))
               { gen = Right <$> wordGen })
         , ("Maybe Word"      , runTest $ cfg { gen = wordGen })
         ]

