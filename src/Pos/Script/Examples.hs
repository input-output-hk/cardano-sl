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

         -- * Stress testing
       , shaStressRedeemer
       , sigStressRedeemer
       ) where

import qualified Data.ByteString    as BS
import           Formatting         (build, sformat, (%))
import           NeatInterpolation  (text)
import           Universum

import           Pos.Binary.Core    ()
import           Pos.Crypto         (PublicKey, SafeSigner, SignTag (SignTxIn),
                                     deterministicKeyGen, fullPublicKeyHexF,
                                     fullSignatureHexF, safeSign, signRaw)
import           Pos.Script         (Script, parseRedeemer, parseValidator)
import           Pos.Txp.Core.Types (TxSigData)

fromE :: Either String Script -> Script
fromE = either (error . toText) identity

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

multisigRedeemer :: TxSigData -> [Maybe SafeSigner] -> Script
multisigRedeemer txSigData sks = fromE $ parseRedeemer Nothing [text|
    redeemer : Comp (List (Maybe ByteString)) {
        redeemer = success ${shownSigs} }
    |]
  where
    mkCons Nothing s = sformat ("(Cons Nothing "%build%")") s
    mkCons (Just sig) s = sformat
        ("(Cons (Just #"%fullSignatureHexF%") "%build%")") sig s
    sigs = map (fmap (\k -> safeSign SignTxIn k txSigData)) sks
    shownSigs = foldr mkCons "Nil" sigs

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

----------------------------------------------------------------------------
-- Stress testing
----------------------------------------------------------------------------

-- | Does N rounds of SHA3-256. Should be used with 'idValidator'.
--
-- Actually it does (N div 10) * 10 rounds. Unrolling the loop is done so
-- that more petrol would be spent on hashing and less – on substraction and
-- function calls.
shaStressRedeemer :: Int -> Script
shaStressRedeemer n = fromE $ parseRedeemer Nothing [text|
    shaLoop : Int -> ByteString -> ByteString {
      shaLoop 0 x = x ;
      shaLoop i x = shaLoop (!subtractInt i 1)
        (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256
        (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256 x)))))))))) }

    redeemer : Comp (Comp Int) {
      redeemer = case !equalsByteString #00 (shaLoop ${ns} #00) of {
        True  -> success (failure) ;
        False -> success (success 1) } }
    |]
  where
    ns = show (n `div` 10)

-- | Checks a signature N times. Should be used with 'idValidator'.
sigStressRedeemer :: Int -> Script
sigStressRedeemer n = fromE $ parseRedeemer Nothing [text|
    sigLoop : Int -> Bool {
      sigLoop 0 = True ;
      sigLoop i = case !verifySignature #${keyS} #00 #${sigS} of {
        True  -> sigLoop (!subtractInt i 1) ;
        False -> False } }

    redeemer : Comp (Comp Int) {
      redeemer = case sigLoop ${ns} of {
        False -> success (failure) ;
        True  -> success (success 1) } }
    |]
  where
    ns = show n
    Just (pk, sk) = deterministicKeyGen (BS.replicate 32 0)
    sig = signRaw Nothing sk (BS.pack [0])

    keyS = sformat fullPublicKeyHexF pk
    sigS = sformat fullSignatureHexF sig
