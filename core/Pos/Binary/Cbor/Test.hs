{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pos.Binary.Cbor.Test where

import qualified Codec.CBOR.FlatTerm as CBOR
import           Pos.Binary.Cbor
import           Universum
import           Test.QuickCheck
import           Pos.Core.Fee
import           Pos.Binary.Core.Fee()
import           Pos.Core.Arbitrary()
import           Pos.Binary.Core.Script()
import           Pos.Core.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    } deriving Show

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |]
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| lastName  :: String |],
        Field [| sex       :: Bool   |]
    ]]

u1 :: User
u1 = deserialize $ serialize $ Login "asd" 34

data T = T1 Int | T2 Int Int | Unknown Word8 BS.ByteString
    deriving Show

instance Bi T where
    encode = \case
        T1 a         -> encode (0::Word8)
                     <> (encode . BSL.toStrict $ serialize a)
        T2 a b       -> encode (1::Word8)
                     <> (encode . BSL.toStrict $ serialize (a, b))
        Unknown n bs -> encode n
                     <> encode bs

    decode = decode @Word8 >>= \case
        0 ->         T1 . deserialize . BSL.fromStrict <$> decode
        1 -> uncurry T2 . deserialize . BSL.fromStrict <$> decode
        t -> Unknown t                                 <$> decode

-- Machinery to test we perform "flat" encoding.
hasValidFlatTerm :: Bi a => a -> Bool
hasValidFlatTerm = CBOR.validFlatTerm . CBOR.toFlatTerm . encode

-- | Given a data type which can be generated randomly and for which the CBOR
-- encoding is defined, generates the roundtrip tests.
roundtripProperty :: (Arbitrary a, Eq a, Show a, Bi a) => a -> Property
roundtripProperty (input :: a) = ((deserialize . serialize $ input) :: a) === input

soundInstanceProperty :: (Arbitrary a, Eq a, Show a, Bi a) => Proxy a -> Property
soundInstanceProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  let itRoundtrips = roundtripProperty input
      isFlat       = hasValidFlatTerm input === True
  in itRoundtrips .&&. isFlat

-- | A set of basic yet-useful roundtrips properties to be included as part
-- of a bigger testsuite.
soundInstancesTest :: IO ()
soundInstancesTest = do
  quickCheck (soundInstanceProperty @Coeff Proxy)
  quickCheck (soundInstanceProperty @TxSizeLinear Proxy)
  quickCheck (soundInstanceProperty @TxFeePolicy Proxy)
  quickCheck (soundInstanceProperty @Script Proxy)
  quickCheck (soundInstanceProperty @Timestamp Proxy)
  quickCheck (soundInstanceProperty @EpochIndex Proxy)
  quickCheck (soundInstanceProperty @Coin Proxy)
  quickCheck (soundInstanceProperty @CoinPortion Proxy)
  quickCheck (soundInstanceProperty @LocalSlotIndex Proxy)
  quickCheck (soundInstanceProperty @SlotId Proxy)
  quickCheck (soundInstanceProperty @EpochOrSlot Proxy)
  quickCheck (soundInstanceProperty @SharedSeed Proxy)
  quickCheck (soundInstanceProperty @ChainDifficulty Proxy)
