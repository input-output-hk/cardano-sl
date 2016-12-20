-- | Some example scripts used in tests.
module Pos.Script.Examples
       ( -- * Trivial
         alwaysSuccessValidator
       , alwaysFailureValidator
       , idValidator

         -- * T/F
       , tfValidator
       , goodTfRedeemer
       , badTfRedeemer

         -- * Extra names
       , tfValidatorWithBlah
       , goodTfRedeemerWithBlah
       ) where

import           Universum

import           Pos.Script (Script, parseRedeemer, parseValidator)

----------------------------------------------------------------------------
-- Trivial validators/redeemers
----------------------------------------------------------------------------

alwaysSuccessValidator :: Script
Right alwaysSuccessValidator = parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : forall a . a -> Comp Unit {",
    "  validator x = success Unit }" ]

alwaysFailureValidator :: Script
Right alwaysFailureValidator = parseValidator $ unlines [
    "data Unit = { Unit }",
    "validator : forall a . a -> Comp Unit {",
    "  validator x = failure }" ]

idValidator :: Script
Right idValidator = parseValidator $ unlines [
    "validator : forall a . Comp a -> Comp a {",
    "  validator x = x }" ]

----------------------------------------------------------------------------
-- True/false validator/redeemer pairs
----------------------------------------------------------------------------

tfValidator :: Script
Right tfValidator = parseValidator $ unlines [
    "data Bool = { True | False }",
    "validator : (forall a . a -> a -> a) -> Comp Bool {",
    "  validator f = case f True False of {",
    "    True  -> success True ;",
    "    False -> failure } }" ]

goodTfRedeemer :: Script
Right goodTfRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp (forall a . a -> a -> a) {",
    "  redeemer = success (\\t f -> t) }" ]

badTfRedeemer :: Script
Right badTfRedeemer = parseRedeemer $ unlines [
    "redeemer : Comp (forall a . a -> a -> a) {",
    "  redeemer = success (\\t f -> f) }" ]

----------------------------------------------------------------------------
-- A pair with extra names
----------------------------------------------------------------------------

tfValidatorWithBlah :: Script
Right tfValidatorWithBlah = parseValidator $ unlines [
    "data Bool = { True | False }",
    "validator : (forall a . a -> a -> a) -> Comp Bool {",
    "  validator f = case f True False of {",
    "    True  -> success True ;",
    "    False -> failure } }",
    "blah : forall a . a -> a {",
    "  blah x = x }" ]

goodTfRedeemerWithBlah :: Script
Right goodTfRedeemerWithBlah = parseRedeemer $ unlines [
    "redeemer : Comp (forall a . a -> a -> a) {",
    "  redeemer = success (\\t f -> t) }",
    "blah : forall a . a -> a {",
    "  blah x = x }" ]
