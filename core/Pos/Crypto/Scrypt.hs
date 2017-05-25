-- | Wrapper over scrypt library

module Pos.Crypto.Scrypt
    ( S.ScryptParams
    , S.Salt (..)
    , S.Pass (..)
    , S.EncryptedPass (..)

    , S.scryptParams
    , S.scryptParamsLen

    , mkSalt
    , genSalt
    , encryptPass
    , encryptPassWithSalt
    , verifyPass
    ) where

import           Universum

import           Crypto.Random    (MonadRandom, getRandomBytes)
import qualified Crypto.Scrypt    as S

import           Pos.Binary.Class (Bi, serialize')

mkSalt :: Bi salt => salt -> S.Salt
mkSalt = S.Salt . serialize'

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
