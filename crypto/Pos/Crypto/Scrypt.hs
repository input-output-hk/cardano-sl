-- | Wrapper over scrypt library.
--
-- Scrypt is a password-based key derivation function — that is,
-- it takes a password as input and produces a (potentially longer)
-- bytestring as output.
--
-- Mostly used in 'Pos.Crypto.Signing.Types.Safe'.

module Pos.Crypto.Scrypt
       ( S.ScryptParams
       , S.Salt (..)
       , S.Pass (..)
       , S.EncryptedPass (..)

       , ScryptParamsBuilder (..)
       , mkScryptParams

       , mkSalt
       , emptySalt
       , genSalt
       , encryptPass
       , encryptPassWithSalt
       , verifyPass
       ) where

import           Universum

import           Crypto.Random (MonadRandom, getRandomBytes)
import qualified Crypto.Scrypt as S
import           Data.Default (Default (..))

import           Pos.Binary.Class (Bi, serialize')

-- | This corresponds to 'ScryptParams' datatype.
-- These parameters influence on resulting hash length, memory and time
-- consumption. See documentation for exact details.
data ScryptParamsBuilder = ScryptParamsBuilder
    { spLogN    :: Word
    , spR       :: Word
    , spP       :: Word
    , spHashLen :: Word
    }

mkScryptParams :: ScryptParamsBuilder -> Maybe S.ScryptParams
mkScryptParams ScryptParamsBuilder {..} =
    S.scryptParamsLen
        (fromIntegral spLogN)
        (fromIntegral spR)
        (fromIntegral spP)
        (fromIntegral spHashLen)

instance Default ScryptParamsBuilder where
    def = ScryptParamsBuilder { spLogN = 14, spR = 8, spP = 1, spHashLen = 64 }

mkSalt :: Bi salt => salt -> S.Salt
mkSalt = S.Salt . serialize'

-- | Salt which can be used for hardcoded values.
emptySalt :: S.Salt
emptySalt = mkSalt ("" :: Text)

genSalt :: MonadRandom m => m S.Salt
genSalt = S.Salt <$> getRandomBytes 32

mkPass :: Bi pass => pass -> S.Pass
mkPass = S.Pass . serialize'

encryptPass
    :: (Bi pass, MonadRandom m)
    => S.ScryptParams -> pass -> m S.EncryptedPass
encryptPass params passphrase =
    genSalt <&> \salt -> encryptPassWithSalt params salt passphrase

encryptPassWithSalt :: Bi pass => S.ScryptParams -> S.Salt -> pass -> S.EncryptedPass
encryptPassWithSalt params salt passphrase =
    S.encryptPass params salt (mkPass passphrase)

verifyPass :: Bi pass => S.ScryptParams -> pass -> S.EncryptedPass -> Bool
verifyPass params passphrase = fst . S.verifyPass params (mkPass passphrase)
