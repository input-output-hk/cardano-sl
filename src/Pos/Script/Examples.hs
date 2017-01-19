{-# LANGUAGE QuasiQuotes #-}

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

         -- * Multisig
       , multisigValidator
       , multisigRedeemer

         -- * Extra names
       , intValidatorWithBlah
       , goodIntRedeemerWithBlah
       ) where

import           Formatting        (build, sformat, (%))
import           NeatInterpolation (text)
import           Universum

import           Pos.Binary.Script ()
import           Pos.Crypto        (PublicKey, SecretKey, fullPublicKeyHexF,
                                    fullSignatureHexF, sign)
import           Pos.Script        (Script, parseRedeemer, parseValidator)
import           Pos.Types.Types   (TxSigData)

fromE :: Either String Script -> Script
fromE = either (panic . toText) identity

----------------------------------------------------------------------------
-- Trivial validators/redeemers
----------------------------------------------------------------------------

alwaysSuccessValidator :: Script
alwaysSuccessValidator = fromE $ parseValidator [text|
    validator : Int -> Comp Unit {
        validator x = success MkUnit }
    |]

alwaysFailureValidator :: Script
alwaysFailureValidator = fromE $ parseValidator [text|
    validator : Int -> Comp Unit {
        validator x = failure }
    |]

idValidator :: Script
idValidator = fromE $ parseValidator [text|
    validator : Comp Int -> Comp Int {
        validator x = x }
    |]

----------------------------------------------------------------------------
-- Int validator/redeemer pairs
----------------------------------------------------------------------------

intValidator :: Script
intValidator = fromE $ parseValidator [text|
    data Foo = { Foo }

    validator : Int -> Comp Foo {
        validator x = case x of {
            1 -> success Foo ;
            _ -> failure } }
    |]

goodIntRedeemer :: Script
goodIntRedeemer = fromE $ parseRedeemer (Just intValidator) [text|
    redeemer : Comp Int {
        redeemer = success 1 }
    |]

badIntRedeemer :: Script
badIntRedeemer = fromE $ parseRedeemer (Just intValidator) [text|
    redeemer : Comp Int {
        redeemer = success 0 }
    |]

----------------------------------------------------------------------------
-- A pair that uses stdlib
----------------------------------------------------------------------------

stdlibValidator :: Script
stdlibValidator = fromE $ parseValidator [text|
    validator : Bool -> Comp Bool {
        validator x = case not (not x) of {
            True -> success True ;
            _    -> failure } }
    |]

goodStdlibRedeemer :: Script
goodStdlibRedeemer = fromE $ parseRedeemer (Just stdlibValidator) [text|
    redeemer : Comp Bool {
        redeemer = success (not False) }
    |]

----------------------------------------------------------------------------
-- Multisig
----------------------------------------------------------------------------

-- verifyMultiSig : Int                          -- how many sigs are needed
--               -> List ByteString              -- allowed pubkeys
--               -> ByteString                   -- txSigData
--               -> List (Maybe ByteString)      -- sigs
--               -> Comp Unit

multisigValidator :: Int -> [PublicKey] -> Script
multisigValidator n pks = fromE $ parseValidator [text|
    validator : List (Maybe ByteString) -> Comp Unit {
        validator sigs = do {
            txSigData <- !transactionInfo;
            verifyMultiSig ${shownN} ${shownPks} txSigData sigs } }
    |]
  where
    shownN = show n
    mkCons k s = sformat ("(Cons #"%fullPublicKeyHexF%" "%build%")") k s
    shownPks = foldr mkCons "Nil" pks

multisigRedeemer :: TxSigData -> [Maybe SecretKey] -> Script
multisigRedeemer txSigData sks = fromE $ parseRedeemer Nothing [text|
    redeemer : Comp (List (Maybe ByteString)) {
        redeemer = success ${shownSigs} }
    |]
  where
    mkCons Nothing s = sformat ("(Cons Nothing "%build%")") s
    mkCons (Just sig) s = sformat
        ("(Cons (Just #"%fullSignatureHexF%") "%build%")") sig s
    shownSigs = foldr mkCons "Nil" (map (fmap (`sign` txSigData)) sks)

----------------------------------------------------------------------------
-- A pair with extra names
----------------------------------------------------------------------------

intValidatorWithBlah :: Script
intValidatorWithBlah = fromE $ parseValidator [text|
    data Foo = { Foo }

    validator : Int -> Comp Foo {
      validator x = case x of {
        1 -> success Foo ;
        _ -> failure } }

    blah : Int -> Int {
      blah x = x }
    |]

goodIntRedeemerWithBlah :: Script
goodIntRedeemerWithBlah = fromE $ parseRedeemer Nothing [text|
    redeemer : Comp Int {
      redeemer = success 1 }

    blah : Int -> Int {
      blah x = x }
    |]
