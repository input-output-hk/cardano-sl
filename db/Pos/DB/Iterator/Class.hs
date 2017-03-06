{-# LANGUAGE TypeFamilies #-}

module Pos.DB.Iterator.Class
       (
         DBIteratorClass (..)
       , IterType
       ) where

import           Universum

class DBIteratorClass i where
    type IterKey   i :: *
    type IterValue i :: *
    iterKeyPrefix :: Proxy i -> ByteString

type IterType i = (IterKey i, IterValue i)
