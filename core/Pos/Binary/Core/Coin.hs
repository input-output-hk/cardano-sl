{-# LANGUAGE ScopedTypeVariables #-}
module Pos.Binary.Core.Coin () where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Types   (Coin, mkCoin, unsafeGetCoin)

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
    decode = mkCoin <$> decode
