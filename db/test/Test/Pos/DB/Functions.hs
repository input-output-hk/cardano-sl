{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Test.Pos.DB.Functions
       ( tests
       ) where

import           Universum

import           Hedgehog

import           Pos.Binary.Class (Bi)
import           Pos.DB (DBTag (..), dbGetBi, dbPutBi)

import           Test.Pos.Chain.Ssc.Example (exampleSscPayload)
import           Test.Pos.Chain.Update.Example (exampleBlockVersionData0)
import           Test.Pos.DB.Mode (runTestMode)


--------------------------------------------------------------------------------
-- | Trying to read a missing key results in a @Nothing@ value
--
prop_missingKey :: Property
prop_missingKey = withTests 1 . property $ do
    result :: Maybe Bool <- liftIO . runTestMode $ dbGetBi MiscDB "test/bool"
    result === Nothing


--------------------------------------------------------------------------------
-- | We can write values into the database and read them back
--
prop_putGet :: Property
prop_putGet = withTests 1 . property $ do
    putGetProperty "test/bool" True
    putGetProperty "test/int" (10000 :: Int)
    putGetProperty "test/bytestring" ("testing" :: ByteString)
    putGetProperty "test/blockversiondata" exampleBlockVersionData0
    putGetProperty "test/sscpayload" exampleSscPayload


--------------------------------------------------------------------------------
-- | We can write values with an explicit version and read them back
--
prop_putGetExplicitVersion :: Property
prop_putGetExplicitVersion = withTests 1 . property $ do
    putGetExplicitVersionProperty "test/bool"       True
    putGetExplicitVersionProperty "test/int"        (10000 :: Int)
    putGetExplicitVersionProperty "test/bytestring" ("testing" :: ByteString)
    putGetExplicitVersionProperty
        "test/blockversiondata"
        exampleBlockVersionData0
    putGetExplicitVersionProperty "test/sscpayload" exampleSscPayload


--------------------------------------------------------------------------------
-- | We can write tuples with @Word8@s and read them back, not interpreting the
--   @Word8@ as a version number
--
prop_putGetWord8Tuple :: Property
prop_putGetWord8Tuple = withTests 1 . property $ do
    putGetWord8TupleProperty "test/bool"             True
    putGetWord8TupleProperty "test/int"              (10000 :: Int)
    putGetWord8TupleProperty "test/bytestring"       ("testing" :: ByteString)
    putGetWord8TupleProperty "test/blockversiondata" exampleBlockVersionData0
    putGetWord8TupleProperty "test/sscpayload"       exampleSscPayload


--------------------------------------------------------------------------------
-- Hedgehog Helpers
--------------------------------------------------------------------------------

putGetProperty
    :: (Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetProperty k v = do
    result <- liftIO . runTestMode $ do
        dbPutBi MiscDB k v
        dbGetBi MiscDB k
    result === Just v

putGetExplicitVersionProperty
    :: (Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetExplicitVersionProperty k v = do
    result <- liftIO . runTestMode $ do
        dbPutBi MiscDB k (0 :: Word8, v)
        dbGetBi MiscDB k
    result === Just v

putGetWord8TupleProperty
    :: (Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetWord8TupleProperty k v = do
    let v' = (0 :: Word8, v)
    result <- liftIO . runTestMode $ do
        dbPutBi MiscDB k v'
        dbGetBi MiscDB k
    result === Just v'


--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkSequential $$(discover)
