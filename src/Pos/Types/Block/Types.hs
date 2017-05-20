{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Block.Types
       ( BiSsc
       , BlockHeader
       , Block
       , BiHeader

       , module Pos.Block.Core.Genesis
       , module Pos.Block.Core.Main
       ) where

import           Universum

import           Pos.Binary.Class       (Bi)
import           Pos.Block.Core.Genesis
import           Pos.Block.Core.Main
import           Pos.Core.Block         (GenericBlockHeader (..))
import           Pos.Ssc.Class.Types    (Ssc (..))

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
