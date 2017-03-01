{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Binary.Infra.Communication () where

import           Data.Reflection                (Reifies (..))
import           Universum

import           Pos.Binary.Class               (Bi (..), limitGet)
import           Pos.Communication.Limits.Types (LimitedLength (..),
                                                 LimitedLengthExt (..), Limiter)

instance (Bi a, Reifies s l, Limiter l) => Bi (LimitedLengthExt s l a) where
    put (LimitedLength a) = put a
    get = undefined
    -- get = do
    --     let maxBlockSize = reflect (Proxy @s)
    --     limitGet maxBlockSize $ LimitedLength <$> get
