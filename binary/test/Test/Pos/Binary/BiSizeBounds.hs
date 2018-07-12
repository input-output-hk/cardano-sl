{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Binary.BiSizeBounds
    ( tests
    ) where

import           Pos.Binary.Class
import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Tagged (Tagged (..))
import           Data.Typeable (typeRep)
import           Hedgehog (Group (..), checkParallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Pos.Binary.Helpers

tests :: IO Bool
tests =
    let listOf = Gen.list (Range.linear 0 300)
        wordGen = Gen.word Range.exponentialBounded
    in checkParallel $ Group "Encoded size bounds for core types."
       $ [ ("()"     , sizeTest $ scfg { gen = pure (), precise = True })
         , ("Bool"   , sizeTest $ cfg { gen = Gen.bool, precise = True })
         , ("Char"   , sizeTest $ cfg { gen = Gen.unicode })
         , ("Char 2" , sizeTest $ cfg { gen = Gen.latin1 })
         , ("String"   , sizeTest $ cfg { gen = listOf Gen.unicode
                                              , lengthOf = Just length
                                              , lengthTy = typeRep (Proxy @(LengthOf [Char])) })
         , ("String 2" , sizeTest $ cfg { gen = listOf Gen.latin1
                                              , lengthOf = Just length
                                              , lengthTy = typeRep (Proxy @(LengthOf [Char])) })
         , ("Word"   , sizeTest $ cfg { gen = Gen.word   Range.exponentialBounded })
         , ("Word8"  , sizeTest $ cfg { gen = Gen.word8  Range.exponentialBounded })
         , ("Word16" , sizeTest $ cfg { gen = Gen.word16 Range.exponentialBounded })
         , ("Word32" , sizeTest $ cfg { gen = Gen.word32 Range.exponentialBounded })
         , ("Word64" , sizeTest $ cfg { gen = Gen.word64 Range.exponentialBounded })
         , ("Int"    , sizeTest $ cfg { gen = Gen.int    Range.exponentialBounded })
         , ("Float"  , sizeTest $ cfg { gen = Gen.float (Range.exponentialFloat (-1000000) 1000000) })
         , ("Int32"  , sizeTest $ cfg { gen = Gen.int32  Range.exponentialBounded })
         , ("Int64"  , sizeTest $ cfg { gen = Gen.int64  Range.exponentialBounded })
         , ("Tagged () Word32", sizeTest $ (scfg @(Tagged () Word32))
               { gen = Tagged <$> Gen.word32 Range.exponentialBounded })
         , ("(Char, Bool)",
               sizeTest $ scfg { gen = (,) <$> Gen.unicode <*> Gen.bool })
         , ("(Char, Char, Bool)",
               sizeTest $ scfg { gen = ((,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool) })
         , ("(Char, Char, Bool, Bool)",
               sizeTest $ scfg { gen = ((,,,) <$> Gen.unicode <*> Gen.unicode <*> Gen.bool <*> Gen.bool) })
         , ("ByteString"     , sizeTest $ (scfg @BS.ByteString)
               { debug = show . (BS.unpack :: BS.ByteString -> [Word8])
               , lengthOf = Just (fromIntegral . BS.length)
               , gen = Gen.bytes (Range.linear 0 1000) })
         , ("Lazy.ByteString", sizeTest $ (scfg @LBS.ByteString)
               { debug = show . (LBS.unpack :: LBS.ByteString -> [Word8])
               , lengthOf = Just (fromIntegral . LBS.length)
               , gen = LBS.fromStrict <$> Gen.bytes (Range.linear 0 1000) })
         , ("Text", sizeTest $ cfg
               { lengthOf = Just length
               , lengthTy = typeRep (Proxy @(LengthOf [Char]))
               , gen = Gen.text (Range.linear 0 1000) Gen.latin1 })
         , ("Text 2", sizeTest $ cfg
               { lengthOf = Just length
               , lengthTy = typeRep (Proxy @(LengthOf [Char]))
               , gen = Gen.text (Range.linear 0 1000) Gen.unicode })
         , ("[Bool]"       , sizeTest $ scfg
               { gen = listOf Gen.bool
               , lengthOf = Just length
               , precise = True})
         , ("NonEmpty Bool", sizeTest $ scfg
               { gen = listOf Gen.bool
               , lengthOf = Just length
               , precise = True })
         , ("Either Bool Word", sizeTest $ (scfg @(Either Bool Word))
               { gen = Left  <$> Gen.bool })
         , ("Either Bool Word", sizeTest $ (scfg @(Either Bool Word))
               { gen = Right <$> wordGen })
         , ("Maybe Word"      , sizeTest $ cfg { gen = wordGen })
         ]

