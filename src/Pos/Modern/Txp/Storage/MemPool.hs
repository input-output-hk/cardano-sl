{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pos.Modern.Txp.Storage.MemPool
       (
         MemPool (..)
       ) where
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Types           (Tx, TxId, TxWitness)


type TxMap = HashMap TxId (Tx, TxWitness)

data MemPool = MemPool
    {
      localTxs     :: !TxMap
    , -- | @length@ is @O(n)@ for 'HM.HashMap' so we store it explicitly.
      localTxsSize :: !Int
    }

instance Default MemPool where
    def = MemPool
        {
          localTxs = HM.empty
        , localTxsSize = 0
        }

