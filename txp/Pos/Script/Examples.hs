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

import qualified Data.ByteString as BS
import           Formatting (build, sformat, (%))
import           NeatInterpolation (text)
import           Serokell.Util.Base16 (base16F)
import           Universum

import           Pos.Binary.Core ()
import           Pos.Core (StakeholderId, TxSigData)
import           Pos.Crypto (SafeSigner, SignTag (SignTx), deterministicKeyGen, fullPublicKeyHexF,
                             fullSignatureHexF, hashHexF, safeSign, safeToPublic, signRaw, signTag)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Script (Script, parseRedeemer, parseValidator)

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
        validator x = case !equalsInt x 1 of {
            True  -> success Foo ;
            False -> failure } }
    |]

goodIntRedeemer :: Script
goodIntRedeemer = fromE $ parseRedeemer [text|
    redeemer : Comp Int {
        redeemer = success 1 }
    |]

badIntRedeemer :: Script
badIntRedeemer = fromE $ parseRedeemer [text|
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
            True  -> success True ;
            False -> failure } }
    |]

goodStdlibRedeemer :: Script
goodStdlibRedeemer = fromE $ parseRedeemer [text|
    redeemer : Comp Bool {
        redeemer = success (not False) }
    |]

----------------------------------------------------------------------------
-- Multisig
----------------------------------------------------------------------------

-- verifyMultiSig
--    : Int                         -- how many valid sigs are needed
--   -> List BS                     -- pubkey hashes that are allowed to vote
--   -> BS                          -- txsigdata
--   -> List (Maybe (Pair BS BS))   -- pubkeys + sigs
--   -> Comp Unit


-- #5820 is prefix for encoded bytestrings (of length 32)
multisigValidator :: HasProtocolMagic => Int -> [StakeholderId] -> Script
multisigValidator n ids = fromE $ parseValidator [text|
    validator : List (Maybe (Pair ByteString ByteString)) -> Comp Unit {
        validator sigs = verifyMultiSig
            ${shownN} ${shownIds}
            (!concatenate ${shownTag}
                (!concatenate #5820 (txhash)
                )
            )
            sigs }
    |]
  where
    shownN = show n
    mkCons h s = sformat ("(Cons #"%hashHexF%" "%build%")") h s
    shownIds = foldr mkCons "Nil" ids
    shownTag = sformat ("#"%base16F) (signTag protocolMagic SignTx)

multisigRedeemer :: HasProtocolMagic => TxSigData -> [Maybe SafeSigner] -> Script
multisigRedeemer txSigData sks = fromE $ parseRedeemer [text|
    redeemer : Comp (List (Maybe (Pair ByteString ByteString))) {
        redeemer = success ${shownSigs} }
    |]
  where
    mkCons Nothing          s = sformat ("(Cons Nothing "%build%")") s
    mkCons (Just (pk, sig)) s = sformat
        ("(Cons (Just (MkPair #"%fullPublicKeyHexF%" #"%fullSignatureHexF%")) "
                %build%")") pk sig s
    sigs = map (fmap (\k -> (safeToPublic k, safeSign protocolMagic SignTx k txSigData))) sks
    shownSigs = foldr mkCons "Nil" sigs

----------------------------------------------------------------------------
-- A pair with extra names
----------------------------------------------------------------------------

intValidatorWithBlah :: Script
intValidatorWithBlah = fromE $ parseValidator [text|
    data Foo = { Foo }

    validator : Int -> Comp Foo {
      validator x = case !equalsInt x 1 of {
        True  -> success Foo ;
        False -> failure } }

    blah : Int -> Int {
      blah x = x }
    |]

goodIntRedeemerWithBlah :: Script
goodIntRedeemerWithBlah = fromE $ parseRedeemer [text|
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
shaStressRedeemer n = fromE $ parseRedeemer [text|
    shaLoop : Int -> ByteString -> ByteString {
      shaLoop i x = case !equalsInt i 0 of {
        True  -> x ;
        False -> shaLoop (!subtractInt i 1)
                     (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256
                     (!sha3_256 (!sha3_256 (!sha3_256 (!sha3_256
                     (!sha3_256 (!sha3_256 x)))))))))) } }

    redeemer : Comp (Comp Int) {
      redeemer = success (
        case !equalsByteString #00 (shaLoop ${ns} #00) of {
          True  -> failure ;
          False -> success 1 } ) }
    |]
  where
    ns = show (n `div` 10)

-- | Checks a signature N times. Should be used with 'idValidator'.
sigStressRedeemer :: HasProtocolMagic => Int -> Script
sigStressRedeemer n = fromE $ parseRedeemer [text|
    sigLoop : Int -> Bool {
      sigLoop i = case !equalsInt i 0 of {
        True  -> True ;
        False -> case !verifySignature #${keyS} #00 #${sigS} of {
          True  -> sigLoop (!subtractInt i 1) ;
          False -> False } } }

    redeemer : Comp (Comp Int) {
      redeemer = success (
        case sigLoop ${ns} of {
          False -> failure ;
          True  -> success 1 } ) }
    |]
  where
    ns = show n
    (pk, sk) = deterministicKeyGen (BS.replicate 32 0)
    sig = signRaw protocolMagic Nothing sk (BS.pack [0])

    keyS = sformat fullPublicKeyHexF pk
    sigS = sformat fullSignatureHexF sig
