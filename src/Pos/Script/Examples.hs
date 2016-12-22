-- | Some example scripts used in tests.
module Pos.Script.Examples
       ( -- * Trivial
         alwaysSuccessValidator
       , alwaysFailureValidator
       , idValidator

         -- * Int
       , intValidator
       , goodIntRedeemer
       , badIntRedeemer

         -- * Stdlib
       , stdlibValidator
       , goodStdlibRedeemer

         -- * Extra names
       , intValidatorWithBlah
       , goodIntRedeemerWithBlah
       ) where

import           Data.String (String)
import           Universum

import           Pos.Script  (Script, parseRedeemer, parseValidator)

fromE :: Either String Script -> Script
fromE = either (panic . toText) identity

----------------------------------------------------------------------------
-- Trivial validators/redeemers
----------------------------------------------------------------------------

alwaysSuccessValidator :: Script
alwaysSuccessValidator = fromE $ parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : Int -> Comp Unit {",
    "  validator x = success Unit }" ]

alwaysFailureValidator :: Script
alwaysFailureValidator = fromE $ parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : Int -> Comp Unit {",
    "  validator x = failure }" ]

idValidator :: Script
idValidator = fromE $ parseValidator $ unlines [
    "validator : Comp Int -> Comp Int {",
    "  validator x = x }" ]

----------------------------------------------------------------------------
-- Int validator/redeemer pairs
----------------------------------------------------------------------------

intValidator :: Script
intValidator = fromE $ parseValidator $ unlines [
    "data Foo = { Foo }",
    "validator : Int -> Comp Foo {",
    "  validator x = case x of {",
    "    1 -> success Foo ;",
    "    _ -> failure } }" ]

goodIntRedeemer :: Script
goodIntRedeemer = fromE $ parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 1 }" ]

badIntRedeemer :: Script
badIntRedeemer = fromE $ parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 0 }" ]

----------------------------------------------------------------------------
-- A pair that uses stdlib
----------------------------------------------------------------------------

stdlibValidator :: Script
stdlibValidator = fromE $ parseValidator $ unlines [
    "validator : Bool -> Comp Bool {",
    "  validator x = case not (not x) of {",
    "    True -> success True ;",
    "    _    -> failure } }" ]

goodStdlibRedeemer :: Script
goodStdlibRedeemer = fromE $ parseRedeemer $ unlines [
    "redeemer : Comp Bool {",
    "  redeemer = success (not False) }" ]

----------------------------------------------------------------------------
-- A pair with extra names
----------------------------------------------------------------------------

intValidatorWithBlah :: Script
intValidatorWithBlah = fromE $ parseValidator $ unlines [
    "data Foo = { Foo }",
    "validator : Int -> Comp Foo {",
    "  validator x = case x of {",
    "    1 -> success Foo ;",
    "    _ -> failure } }",
    "blah : Int -> Int {",
    "  blah x = x }" ]

goodIntRedeemerWithBlah :: Script
goodIntRedeemerWithBlah = fromE $ parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 1 }",
    "blah : Int -> Int {",
    "  blah x = x }" ]
