module Pos.Binary.Core.Common () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Core.Common.Types as T
import qualified Pos.Data.Attributes as A
import           Pos.Util.Orphans ()

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi (A.Attributes ()) where
    encode = A.encodeAttributes []
    decode = A.decodeAttributes () $ \_ _ _ -> pure Nothing

instance Bi T.CoinPortion where
    encode = encode . T.getCoinPortion
    decode = T.UncheckedCoinPortion <$> decode

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

instance Bi T.Coin where
    encode = encode . T.unsafeGetCoin
    decode = T.UncheckedCoin <$> decode
