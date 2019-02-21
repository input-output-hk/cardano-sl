module Pos.DB.GState.CommonOp
       ( CommonOp (..)
       ) where

import qualified Database.RocksDB as Rocks
import           Formatting (bprint, int, (%))
import           Formatting.Buildable (Buildable (build))

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Block (HeaderHash)
import           Pos.Core (ChainDifficulty)
import           Pos.Crypto (shortHashF)
import           Pos.DB.BatchOp (RocksBatchOp (..))
import           Pos.DB.GState.ChainDifficulty (maxSeenDifficultyKey)
import           Pos.DB.GState.Tip (tipKey)

data CommonOp = PutTip HeaderHash | PutMaxSeenDifficulty ChainDifficulty

instance Buildable CommonOp where
    build (PutTip h) =
        bprint ("PutTip ("%shortHashF%")") h
    build (PutMaxSeenDifficulty d) =
        bprint ("PutMaxSeenDifficulty ("%int%")") d

instance RocksBatchOp CommonOp where
    toBatchOp (PutTip h) =
        [Rocks.Put tipKey (serialize' h)]
    toBatchOp (PutMaxSeenDifficulty h) =
        [Rocks.Put maxSeenDifficultyKey (serialize' h)]
