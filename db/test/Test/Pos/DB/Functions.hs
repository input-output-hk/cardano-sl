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
import           Pos.Core (HasConfiguration, withCoreConfiguration)
import           Pos.DB (DBTag (..), dbGetBi, dbGetBiNoVersion, dbPutBi,
                     dbPutBiNoVersion)

import           Test.Pos.Core.Dummy (dummyCoreConfiguration)
import           Test.Pos.Core.ExampleHelpers (exampleBlockVersionData,
                     exampleSscPayload)
import           Test.Pos.DB.Mode (runTestMode)


--------------------------------------------------------------------------------
-- | Trying to read a missing key results in a @Nothing@ value
--
prop_missingKey :: Property
prop_missingKey = withTests 1 $ dbProperty $ do
    result :: Maybe Bool <- liftIO . runTestMode $ dbGetBi MiscDB "test/bool"
    result === Nothing


--------------------------------------------------------------------------------
-- | We can write values into the database and read them back
--
prop_putGet :: Property
prop_putGet = withTests 1 $ dbProperty $ do
    putGetProperty "test/bool" True
    putGetProperty "test/int" (10000 :: Int)
    putGetProperty "test/bytestring" ("testing" :: ByteString)
    putGetProperty "test/blockversiondata" exampleBlockVersionData
    putGetProperty "test/sscpayload" exampleSscPayload


--------------------------------------------------------------------------------
-- | We can write values with an explicit version and read them back
--
prop_putGetExplicitVersion :: Property
prop_putGetExplicitVersion = withTests 1 $ dbProperty $ do
    putGetExplicitVersionProperty "test/bool"       True
    putGetExplicitVersionProperty "test/int"        (10000 :: Int)
    putGetExplicitVersionProperty "test/bytestring" ("testing" :: ByteString)
    putGetExplicitVersionProperty
        "test/blockversiondata"
        exampleBlockVersionData
    putGetExplicitVersionProperty "test/sscpayload" exampleSscPayload


--------------------------------------------------------------------------------
-- | We can write values with no version and read them back
--
prop_putGetNoVersion :: Property
prop_putGetNoVersion = withTests 1 $ dbProperty $ do
    putGetNoVersionProperty "test/bool"             True
    putGetNoVersionProperty "test/int"              (10000 :: Int)
    putGetNoVersionProperty "test/bytestring"       ("testing" :: ByteString)
    putGetNoVersionProperty "test/blockversiondata" exampleBlockVersionData
    putGetNoVersionProperty "test/sscpayload"       exampleSscPayload


--------------------------------------------------------------------------------
-- Hedgehog Helpers
--------------------------------------------------------------------------------

dbProperty :: (HasConfiguration => PropertyT IO ()) -> Property
dbProperty prop = property $ withCoreConfiguration dummyCoreConfiguration prop

putGetProperty
    :: (HasConfiguration, Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetProperty k v = do
    result <- liftIO . runTestMode $ do
        dbPutBi MiscDB k v
        dbGetBi MiscDB k
    result === Just v

putGetExplicitVersionProperty
    :: (HasConfiguration, Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetExplicitVersionProperty k v = do
    result <- liftIO . runTestMode $ do
        dbPutBiNoVersion MiscDB k (0 :: Word8, v)
        dbGetBi MiscDB k
    result === Just v

putGetNoVersionProperty
    :: (HasConfiguration, Bi a, Eq a, Show a)
    => ByteString
    -> a
    -> PropertyT IO ()
putGetNoVersionProperty k v = do
    result <- liftIO . runTestMode $ do
        dbPutBiNoVersion MiscDB k v
        dbGetBiNoVersion MiscDB k
    result === Just v


--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkSequential $$(discover)
