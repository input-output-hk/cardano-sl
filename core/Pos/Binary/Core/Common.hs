module Pos.Binary.Core.Common () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Common.Types (Coin, mkCoin, unsafeGetCoin)
import qualified Pos.Core.Common.Types as T
import qualified Pos.Data.Attributes as A
import           Pos.Util.Orphans ()
import           Pos.Util.Util (toCborError)

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi (A.Attributes ()) where
    encode = A.encodeAttributes []
    decode = A.decodeAttributes () $ \_ _ _ -> pure Nothing

instance Bi T.CoinPortion where
    encode = encode . T.getCoinPortion
    decode =
        T.mkCoinPortion <$> (decode @Word64) >>= toCborError

instance Bi T.BlockCount where
    encode = encode . T.getBlockCount
    decode = T.BlockCount <$> decode

deriveSimpleBi ''T.SharedSeed [
    Cons 'T.SharedSeed [
        Field [| T.getSharedSeed :: ByteString |]
    ]]

deriveSimpleBi ''T.ChainDifficulty [
    Cons 'T.ChainDifficulty [
        Field [| T.getChainDifficulty :: T.BlockCount |]
    ]]

-- | This instance required only for Arbitrary instance of HeaderHash
-- due to @instance Bi a => Hash a@.
instance Bi T.BlockHeaderStub where
    encode = error "somebody tried to binary encode BlockHeaderStub"
    decode = error "somebody tried to binary decode BlockHeaderStub"

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- number of total coins is 45*10^9 * 10^6
--
--  Input                        | Bits to represent |
-- ------------------------------| ----------------- |
-- 0-9                           |      8 bits       |
-- 0-99                          |      16 bits      |
-- 0-999                         |      24 bits      |
-- 0-9999                        |      24 bits      |
-- 0-99999                       |      40 bits      |
-- 0-999999                      |      40 bits      |
-- 45*10^15                      |      72 bits      |
-- 45*10^9                       |      72 bits      |
-- 45*10^9 * 10^6 (maxbound)     |      72 bits      |
-- maxbound - 1                  |      72 bits      |

instance Bi Coin where
    encode = encode . unsafeGetCoin
    decode =
        decode >>= toCborError . \case
            number
                | number > unsafeGetCoin maxBound ->
                    Left $
                    "decode@Coin: number is greater than limit: " <>
                    show number
                | otherwise -> Right (mkCoin number)
