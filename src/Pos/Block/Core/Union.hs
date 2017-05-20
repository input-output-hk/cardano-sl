{-# LANGUAGE TemplateHaskell #-}

module Pos.Block.Core.Union
       ( BiSsc
       , BlockHeader
       , Block
       , BiHeader

       , module Pos.Core.Block
       , module Pos.Block.Core.Genesis
       , module Pos.Block.Core.Main
       ) where

import           Universum

import           Pos.Binary.Class       (Bi)
import           Pos.Ssc.Class.Types    (Ssc (..))

-- Re-exports
import           Pos.Block.Core.Genesis
import           Pos.Block.Core.Main
import           Pos.Core.Block

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Ssc w/ buildable blockchain
type BiSsc ssc =
    ( Ssc ssc
    , Bi (GenericBlockHeader (GenesisBlockchain ssc))
    , Bi (GenericBlockHeader (MainBlockchain ssc)))

-- | Either header of ordinary main block or genesis block.
type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)

type BiHeader ssc = Bi (BlockHeader ssc)

-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)
