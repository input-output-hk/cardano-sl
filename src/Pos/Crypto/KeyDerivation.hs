module Pos.Crypto.KeyDerivation
    ( deriveChildKey
    ) where

import           Pos.Crypto.Signing
import           Crypto.Hash
import           Crypto.Number.Serialize
import           Crypto.MAC.HMAC
import qualified Crypto.Sign.Ed25519    as Ed25519
import qualified Data.ByteString as B (ByteString, splitAt)
import           Data.ByteArray as B hiding (splitAt)
import           Data.ByteArray.Pack as BP
import           Data.Memory.Endian (BE(..))
import           Data.Word
import           Universum

newtype ChainCode = ChainCode B.ByteString

ser32 :: Word32 -> ByteString
ser32 n = either (panic "ser32 impossible") identity $ BP.fill 4 (BP.putStorable (BE n))

-- | Derive Child Key
--
-- Check whether i ≥ 231 (whether the child is a hardened key).
-- If so (hardened child): let I = HMAC-SHA512(Key = cpar, Data = 0x00 || ser256(kpar) || ser32(i)). (Note: The 0x00 pads the private key to make it 33 bytes long.)
-- If not (normal child): let I = HMAC-SHA512(Key = cpar, Data = serP(point(kpar)) || ser32(i)).
--
-- Split I into two 32-byte sequences, IL and IR.
--
-- The returned child key ki is parse256(IL) + kpar (mod n).
-- The returned chain code ci is IR.
--
-- In case parse256(IL) ≥ n or ki = 0, the resulting key is invalid, and one should proceed with the next value for i. (Note: this has probability lower than 1 in 2127.)
deriveChildKey :: SecretKey -> ChainCode -> Word32 -> (SecretKey, ChainCode)
deriveChildKey (SecretKey (Ed25519.SecretKey sec)) cc n =
    let (il, ir) = B.splitAt 32 $ B.convert $ kdf cc (sec `B.append` ser32 n)
        ki = (os2ip il + os2ip sec) `mod` 57896044618658097711785492504343953926634992332820282019728792003956564819949
     in (SecretKey (Ed25519.SecretKey (i2ospOf_ 32 ki)), ChainCode ir)

kdf :: ChainCode -> ByteString -> HMAC SHA512
kdf (ChainCode cc) = hmac cc
