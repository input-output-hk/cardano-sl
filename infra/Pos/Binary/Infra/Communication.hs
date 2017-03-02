{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Binary.Infra.Communication () where

import           Data.Reflection                (Reifies (..))
import           Universum

import           Pos.Binary.Class               (Bi (..))
import           Pos.Communication.Limits.Types (LimitedLengthExt (..), Limiter, limitGet)

instance forall s l a. (Bi a, Reifies s l, Limiter l) =>
         Bi (LimitedLengthExt s l a) where
    put (LimitedLength a) = put a
    get = do
        let maxSize = reflect (Proxy @s)
        limitGet maxSize $ LimitedLength <$> get
