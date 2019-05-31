{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Block.Slog.LastBlkSlots
       ( tests
       ) where

import           Universum

import           Data.ByteString (ByteString)
import qualified Data.List as List

import           Hedgehog (Gen, Property, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Block.Slog.LastBlkSlots (LastBlkSlots,
                     LastSlotInfo (..))
import qualified Pos.Chain.Block.Slog.LastBlkSlots as LastBlkSlots
import           Pos.Core (AddressHash, unsafeAddressHash)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (PublicKey (..))


-- Need a small number of public keys, and for tests, they don't have be proper
-- public keys.
genPublicKeyAddressHash :: Gen (AddressHash PublicKey)
genPublicKeyAddressHash = do
    let pkeys = [ "one", "two", "three", "four", "five", "six", "seven" :: ByteString ]
    unsafeAddressHash <$> Gen.element pkeys

getLastSlotInfo :: Int -> Gen [LastSlotInfo]
getLastSlotInfo len = do
    start <- Gen.word64 $ Range.linear 0 20000
    end <- Gen.word64 $ Range.linear (start + fromIntegral len) (start + 1000)
    -- The 'lsiFlatSlotId' values should be monotonically increasing, but may
    -- have gaps due or slot without blocks.
    lsis <- List.sort . List.take len <$> Gen.shuffle [start .. end]
    mapM (\lsi -> LastSlotInfo lsi <$> genPublicKeyAddressHash) lsis

getNextLastSlotInfo :: [LastSlotInfo] -> Gen LastSlotInfo
getNextLastSlotInfo xs = do
    lsi <- case xs of
                [] -> Gen.word64 $ Range.linear 0 20000
                _  -> pure $ 1 + List.last (map lsiFlatSlotId xs)
    LastSlotInfo (lsi + 1) <$> genPublicKeyAddressHash

genLastBlkSlots :: Gen LastBlkSlots
genLastBlkSlots = do
    count <- Gen.int $ Range.linear 1 100
    full <- Gen.bool
    len <- Gen.int $ if full
                        then Range.linear count (count + 10)
                        else Range.linear 0 (count - 1)
    LastBlkSlots.fromList count <$> fmap OldestFirst (getLastSlotInfo len)

-- Validate basic invariants
isValid :: LastBlkSlots -> Bool
isValid lbs =
    and [ LastBlkSlots.listLength lbs == LastBlkSlots.totalKeyCount lbs
        , LastBlkSlots.listLength lbs <= LastBlkSlots.lbsCount lbs
        , length (ordNub . map lsiLeaderPubkeyHash $ LastBlkSlots.getList lbs)
            == LastBlkSlots.mapSize lbs
        ]

-- -----------------------------------------------------------------------------

-- Make sure the generator generates valid LastBlkSlots.
prop_is_valid :: Property
prop_is_valid =
    H.withTests 1000 . H.property $ do
        lbs <- H.forAll genLastBlkSlots
        let full = LastBlkSlots.isFull lbs
        H.assert $ isValid lbs
        H.cover 30 "  already full" full
        H.cover 30 "  not full" (not full)

prop_LastSlotInfo_list_order :: Property
prop_LastSlotInfo_list_order =
    H.withTests 1000 . H.property $ do
        listLen <- H.forAll $ Gen.int (Range.linear 1 100)
        fsids <- H.forAll $ (lsiFlatSlotId <<$>> getLastSlotInfo listLen)
        fsids === List.sort fsids

-- Adding an element increments the count for non-full LastBlkSlots.
prop_adding_element_increments_count :: Property
prop_adding_element_increments_count =
    H.withTests 200 . H.property $ do
        before <- H.forAll genLastBlkSlots
        next <- H.forAll $ getNextLastSlotInfo (LastBlkSlots.getList before)
        let pkey = lsiLeaderPubkeyHash next
            after = LastBlkSlots.update before next
        H.assert $ isValid after
        let full = LastBlkSlots.isFull before
        unless full $
            (LastBlkSlots.getKeyCount before pkey) + 1 === LastBlkSlots.getKeyCount after pkey
        H.cover 30 "  already full" full
        H.cover 30 "  not full" (not full)

-- The total count (sum of counts for each key) is <= the max count.
prop_total_count_is_LE_count :: Property
prop_total_count_is_LE_count =
    H.withTests 200 . H.property $ do
        before <- H.forAll genLastBlkSlots
        next <- H.forAll $ getNextLastSlotInfo (LastBlkSlots.getList before)
        let after = LastBlkSlots.update before next
        H.assert $ isValid after
        let full = LastBlkSlots.isFull before
        if full
            then LastBlkSlots.totalKeyCount after === LastBlkSlots.lbsCount after
            else (LastBlkSlots.totalKeyCount before) + 1 === LastBlkSlots.totalKeyCount after
        H.cover 30 "  already full" full
        H.cover 30 "  not full" (not full)

-- Make sure the list count agrees with the map.
prop_map_agrees_with_list :: Property
prop_map_agrees_with_list =
    H.withTests 200 . H.property $ do
        lbs <- H.forAll genLastBlkSlots
        addr <- H.forAll genPublicKeyAddressHash
        H.assert $ isValid lbs
        let target = length (filter (\x -> lsiLeaderPubkeyHash x == addr) $
                        LastBlkSlots.getList lbs)
        target === LastBlkSlots.getKeyCount lbs addr
        let full = LastBlkSlots.isFull lbs
        H.cover 30 "  already full" full
        H.cover 30 "  not full" (not full)

-- Make sure the input list ordering is preserved.
prop_list_order_preserved :: Property
prop_list_order_preserved =
    H.withTests 200 . H.property $ do
        lbsSize <- H.forAll $ Gen.int (Range.linear 1 100)
        listLen <- H.forAll $ Gen.int (Range.linear 1 100)
        lsis <- H.forAll $ getLastSlotInfo listLen
        let before = LastBlkSlots.create lbsSize
            after = LastBlkSlots.updateMany before (OldestFirst lsis)
        H.assert $ isValid after
        let longer = listLen > lbsSize
            output = LastBlkSlots.getList after
        if longer
            then output === List.drop (listLen - length output) lsis
            else output === lsis
        H.cover 30 "  longer      " longer
        H.cover 30 "  shorter     " (not longer)


-- Check that the updating with removal gives correct remove list.
prop_update_with_removed_complete :: Property
prop_update_with_removed_complete =
    H.withTests 200 . H.property $ do
        before <- H.forAll genLastBlkSlots
        listLen <- H.forAll $ Gen.int (Range.linear 1 100)
        lsis <- H.forAll $ getLastSlotInfo listLen
        let (after, removed) = LastBlkSlots.updateManyR before (OldestFirst lsis)
        H.assert $ isValid after
        let longer = listLen > LastBlkSlots.lbsCount before
        LastBlkSlots.getList before ++ lsis === getOldestFirst removed ++ LastBlkSlots.getList after
        H.cover 30 "  longer      " longer
        H.cover 30 "  shorter     " (not longer)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
