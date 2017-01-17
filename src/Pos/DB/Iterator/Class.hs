{-# LANGUAGE TypeFamilies #-}

module Pos.DB.Iterator.Class
       (
         MonadDBIterator (..)
       , IterType
       ) where

import           Universum

class MonadDBIterator i where
    type IterKey   i :: *
    type IterValue i :: *
    iterKeyPrefix :: Proxy i -> ByteString

type IterType i = (IterKey i, IterValue i)
