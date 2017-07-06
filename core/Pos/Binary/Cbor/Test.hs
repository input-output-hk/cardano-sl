{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Pos.Binary.Cbor.Test where

import Pos.Binary.Cbor.TH
import Pos.Binary.Cbor.Serialization
import Universum

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
