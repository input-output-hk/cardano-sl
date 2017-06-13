-- | Wrapper over scrypt library

module Pos.Crypto.Scrypt
    ( S.ScryptParams
    , S.Salt (..)
    , S.Pass (..)
    , S.EncryptedPass (..)

    , ScryptParamsBuilder (..)
    , mkScryptParams

    , mkSalt
    , genSalt
    , encryptPassIO
    , encryptPassWithSalt
    , verifyPass
    ) where

import           Universum

import qualified Crypto.Scrypt              as S
import           Data.Default               (Default (..))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi, encodeStrict)
import           Pos.Crypto.Random          (secureRandomBS)

-- | This corresponds to 'ScryptParams' datatype.
-- See <https://hackage.haskell.org/package/scrypt-0.5.0/docs/Crypto-Scrypt.html
-- hashing parameters description> for more details; briefly explained below.
data ScryptParamsBuilder = ScryptParamsBuilder
    { spLogN    :: Word  -- ^ Influences memory & time usage (exponentially)
    , spR       :: Word  -- ^ Influences memory & time usage
    , spP       :: Word  -- ^ Influences only time usage
    , spHashLen :: Byte  -- ^ Length of resulting hash
    }

mkScryptParams :: ScryptParamsBuilder -> Maybe S.ScryptParams
mkScryptParams ScryptParamsBuilder {..} =
    S.scryptParamsLen
        (fromIntegral spLogN)
        (fromIntegral spR)
        (fromIntegral spP)
        (fromIntegral spHashLen)

-- | Corresponds to 'S.defaultParams'
instance Default ScryptParamsBuilder where
    def = ScryptParamsBuilder { spLogN = 14, spR = 8, spP = 1, spHashLen = 64 }

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
