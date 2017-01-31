{-# LANGUAGE UndecidableInstances #-}

-- | Types describing protocol messages related to Blocks.

module Pos.Block.Network.Types
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Serokell.Util.Text  (listJson)
import           Universum

import           Pos.Ssc.Class.Types (Ssc (SscPayload))
import           Pos.Types           (Block, BlockHeader, HeaderHash)
import           Pos.Util            (NE, NewestFirst)

-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: ![HeaderHash]
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetHeaders where
    build (MsgGetHeaders mghFrom mghTo) =
        bprint ("MsgGetHeaders {from = "%listJson%", to = "%build%"}")
               mghFrom (maybe "<Nothing>" (bprint build) mghTo)

-- | 'GetHeaders' message (see protocol specification).
data MsgGetBlocks = MsgGetBlocks
    { mgbFrom :: !HeaderHash
    , mgbTo   :: !HeaderHash
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetBlocks where
    build (MsgGetBlocks mgbFrom mgbTo) =
        bprint ("MsgGetBlocks {from = "%build%", to = "%build%"}")
               mgbFrom mgbTo

-- | 'Headers' message (see protocol specification).
newtype MsgHeaders ssc =
    MsgHeaders (NewestFirst NE (BlockHeader ssc))
    deriving (Generic, Show, Eq)

-- | 'Block' message (see protocol specification).
--
-- The @s@ parameter is used for passing block size limit to deserialization
-- instances (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
newtype MsgBlock s ssc =
    MsgBlock (Block ssc)
    deriving (Generic, Show)

deriving instance (Ssc ssc, Eq (SscPayload ssc)) => Eq (MsgBlock s ssc)
