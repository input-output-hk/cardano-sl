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
    , encryptPassIO
    , encryptPassWithSalt
    , verifyPass
    ) where

import           Universum

import qualified Crypto.Scrypt     as S

import           Pos.Binary.Class  (Bi, encodeStrict)
import           Pos.Crypto.Random (secureRandomBS)

mkSalt :: Bi salt => salt -> S.Salt
mkSalt = S.Salt . encodeStrict

genSalt :: MonadIO m => m S.Salt
genSalt = liftIO $ S.Salt <$> secureRandomBS 32

mkPass :: Bi pass => pass -> S.Pass
mkPass = S.Pass . encodeStrict

encryptPassIO :: (Bi pass, MonadIO m) => S.ScryptParams -> pass -> m S.EncryptedPass
encryptPassIO params passphrase =
    genSalt <&> \salt -> encryptPassWithSalt params salt passphrase

encryptPassWithSalt :: Bi pass => S.ScryptParams -> S.Salt -> pass -> S.EncryptedPass
encryptPassWithSalt params salt passphrase =
    S.encryptPass params salt (mkPass passphrase)

verifyPass :: Bi pass => S.ScryptParams -> pass -> S.EncryptedPass -> Bool
verifyPass params passphrase = fst . S.verifyPass params (mkPass passphrase)
