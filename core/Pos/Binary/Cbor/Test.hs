{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Pos.Binary.Cbor.Test where

import           Pos.Binary.Cbor
import           Universum
import           Data.Fixed
import           Test.QuickCheck hiding (Fixed)
import           Pos.Core.Fee
import           Pos.Binary.Core.Fee()

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

coeffRoundtripProperty :: Property
coeffRoundtripProperty = forAll arbitrary $ \integer ->
  let input = Coeff ((MkFixed integer) :: Nano)
  in ((deserialize . serialize $ input) :: Coeff) === input

roundtrips = do
  quickCheck coeffRoundtripProperty
