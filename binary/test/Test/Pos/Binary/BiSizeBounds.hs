{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Binary.BiSizeBounds
    ( tests
    ) where

import           Pos.Binary.Class
import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import           Data.Tagged (Tagged (..))
import           Data.Typeable (typeRep)
import           Hedgehog (Group (..), checkParallel, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Pos.Binary.Helpers

tests :: IO Bool
tests =
    let listOf = Gen.list (Range.linear 0 300)
        longListOf = Gen.list (Range.linear 0 100000)
    in checkParallel $ Group "Encoded size bounds for core types."
       $ [ ("()"     , sizeTest $ scfg { gen = pure (), precise = True })
         , ("Bool"   , sizeTest $ cfg { gen = Gen.bool, precise = True })
         , ("Char"   , sizeTest $ cfg { gen = Gen.unicode })
         , ("Char 2" , sizeTest $ cfg { gen = Gen.latin1 })
         , ("String"   , sizeTest $ cfg { gen = listOf Gen.unicode
                                        , computedCtx = \str -> M.fromList
                                            [ (typeRep (Proxy @(LengthOf [Char])),
                                               SizeConstant $ fromIntegral $ length str)
                                            ]
                                        })
         , ("String 2" , sizeTest $ cfg { gen = listOf Gen.latin1
                                        , computedCtx = \str -> M.fromList
                                            [ (typeRep (Proxy @(LengthOf [Char])),
                                               SizeConstant $ fromIntegral $ length str)
                                            ]
                                        })
         , ("String 3" , sizeTest $ cfg { gen = listOf Gen.alpha
                                        , addlCtx = M.fromList
                                            [ (typeRep (Proxy @[Char]), SelectCases ["minChar"]) ]
                                        , computedCtx = \str -> M.fromList
                                            [ (typeRep (Proxy @(LengthOf [Char])),
                                               SizeConstant $ fromIntegral $ length str)
                                            ]
                                        , precise = True
                                        })
         , ("String 4" , withTests 20 $ sizeTest $
               cfg { gen = longListOf Gen.alpha
                   , addlCtx = M.fromList
                               [ (typeRep (Proxy @[Char]), SelectCases ["minChar"]) ]
                   , computedCtx = \str -> M.fromList
                                          [ (typeRep (Proxy @(LengthOf [Char])),
                                                SizeConstant $ fromIntegral $ length str)
                                          ]
                   , precise = True
                   })
         , ("Char 3", sizeTest $ cfg { gen = Gen.alpha
                                     , addlCtx = M.fromList
                                         [ (typeRep (Proxy @Char), SizeConstant 2) ]
                                     , precise = True
                                     })
         , ("Word"   , sizeTest $ cfg { gen = Gen.word   Range.exponentialBounded })
         , ("Word8"  , sizeTest $ cfg { gen = Gen.word8  Range.exponentialBounded })
         , ("Word16" , sizeTest $ cfg { gen = Gen.word16 Range.exponentialBounded })
         , ("Word32" , sizeTest $ cfg { gen = Gen.word32 Range.exponentialBounded })
         , ("Word64" , sizeTest $ cfg { gen = Gen.word64 Range.exponentialBounded })
         , ("Int"    , sizeTest $ cfg { gen = Gen.int    Range.exponentialBounded })
         , ("Int (precision)", sizeTest $ cfg
               { gen = Gen.int Range.exponentialBounded
               , computedCtx = \x -> M.fromList
                   [ (typeRep (Proxy @Int), SizeConstant $ fromIntegral (withWordSize x :: Integer)) ]
               , precise = True })
         , ("Float"  , sizeTest $ cfg { gen = Gen.float (Range.exponentialFloat (-1000000) 1000000) })
         , ("Int32"  , sizeTest $ cfg { gen = Gen.int32  Range.exponentialBounded })
         , ("Int64"  , sizeTest $ cfg { gen = Gen.int64  Range.exponentialBounded })
         , ("Tagged () Word32", sizeTest $ (scfg @(Tagged () Word32))
               { gen = Tagged <$> Gen.word32 Range.exponentialBounded })
         , ("(Bool, Bool)",
               sizeTest $ scfg { gen = (,) <$> Gen.bool <*> Gen.bool
                               , precise = True })
         , ("(Bool, Bool, Bool)",
               sizeTest $ scfg { gen = ((,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool)
                               , precise = True })
         , ("(Bool, Bool, Bool, Bool)",
               sizeTest $ scfg { gen = ((,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool)
                               , precise = True})
         , ("ByteString"     , sizeTest $ (scfg @BS.ByteString)
               { debug = show . (BS.unpack :: BS.ByteString -> [Word8])
               , gen = Gen.bytes (Range.linear 0 1000)
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf ByteString)),
                                            SizeConstant $ fromIntegral $ BS.length bs)
                                      ]
               , precise = True })
         , ("Lazy.ByteString", sizeTest $ (scfg @LBS.ByteString)
               { debug = show . (LBS.unpack :: LBS.ByteString -> [Word8])
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf LBS.ByteString)),
                                            SizeConstant $ fromIntegral $ LBS.length bs)
                                      ]
               , gen = LBS.fromStrict <$> Gen.bytes (Range.linear 0 1000)
               , precise = True })
         , ("Text", sizeTest $ cfg
               { gen = Gen.text (Range.linear 0 1000) Gen.latin1
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf [Char])),
                                            SizeConstant $ fromIntegral $ length bs)
                                      ]
               })
         , ("Text 2", sizeTest $ cfg
               { gen = Gen.text (Range.linear 0 1000) Gen.unicode
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf [Char])),
                                            SizeConstant $ fromIntegral $ length bs)
                                      ]
               })
         , ("[Bool]"       , sizeTest $ scfg
               { gen = listOf Gen.bool
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf [Bool])),
                                            SizeConstant $ fromIntegral $ length bs)
                                      ]
               , precise = True})
         , ("NonEmpty Bool", sizeTest $ scfg
               { gen = listOf Gen.bool
               , computedCtx = \bs -> M.fromList
                                      [ (typeRep (Proxy @(LengthOf [Bool])),
                                            SizeConstant $ fromIntegral $ length bs)
                                      ]
               , precise = True })
         , ("Either Bool Bool", sizeTest $ (scfg @(Either Bool Bool))
               { gen = Left  <$> Gen.bool
               , precise = True })
         , ("Either Bool Bool", sizeTest $ (scfg @(Either Bool Bool))
               { gen = Right <$> Gen.bool
               , precise = True })
         , ("Maybe Bool"      , sizeTest $ cfg { gen = Gen.bool
                                               , precise = True })
         ]

