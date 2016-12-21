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

         -- * Extra names
       , intValidatorWithBlah
       , goodIntRedeemerWithBlah
       ) where

import           Universum

import           Pos.Script (Script, parseRedeemer, parseValidator)

----------------------------------------------------------------------------
-- Trivial validators/redeemers
----------------------------------------------------------------------------

alwaysSuccessValidator :: Script
Right alwaysSuccessValidator = parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : Int -> Comp Unit {",
    "  validator x = success Unit }" ]

alwaysFailureValidator :: Script
Right alwaysFailureValidator = parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : Int -> Comp Unit {",
    "  validator x = failure }" ]

idValidator :: Script
Right idValidator = parseValidator $ unlines [
    "validator : Comp Int -> Comp Int {",
    "  validator x = x }" ]

----------------------------------------------------------------------------
-- Int validator/redeemer pairs
----------------------------------------------------------------------------

intValidator :: Script
Right intValidator = parseValidator $ unlines [
    "data Bool = { True | False }",
    "validator : Int -> Comp Bool {",
    "  validator x = case x of {",
    "    1 -> success True ;",
    "    _ -> failure } }" ]

goodIntRedeemer :: Script
Right goodIntRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 1 }" ]

badIntRedeemer :: Script
Right badIntRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 0 }" ]

----------------------------------------------------------------------------
-- A pair with extra names
----------------------------------------------------------------------------

intValidatorWithBlah :: Script
Right intValidatorWithBlah = parseValidator $ unlines [
    "data Bool = { True | False }",
    "validator : Int -> Comp Bool {",
    "  validator x = case x of {",
    "    1 -> success True ;",
    "    _ -> failure } }",
    "blah : Int -> Int {",
    "  blah x = x }" ]

goodIntRedeemerWithBlah :: Script
Right goodIntRedeemerWithBlah = parseRedeemer $ unlines [
    "redeemer : Comp Int {",
    "  redeemer = success 1 }",
    "blah : Int -> Int {",
    "  blah x = x }" ]
