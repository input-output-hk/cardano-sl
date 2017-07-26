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

import           Pos.Block.Core      (Block, BlockHeader)
import           Pos.Core            (HeaderHash)
import           Pos.Ssc.Class.Types (Ssc (SscPayload))
import           Pos.Util.Chrono     (NE, NewestFirst)

-- | 'GetHeaders' message. Behaviour of the response depends on
-- particular combination of 'mghFrom' and 'mghTo'.
--
-- * 'mghTo' resolves to some header (let's call it @top@ for
-- convenience) -- node's tip if it's @Nothing@, header with hash in
-- @Just@ if it's @Just@.
--
-- * If 'mghFrom' is empty, then semantics is "request to return
-- header of block @top@".
--
-- * Otherwise (if 'mghFrom' isn't empty) it represents the set of
-- checkpoints. Responding node will try to iterate headers from @top@
-- to older until it reaches any checkpoint. If it finds checkpoint
-- @c@, it returns all headers in range @[c.next..top]@. If it doesn't
-- find any checkpoint or depth of searching exceeds
-- 'recoveryHeadersMessage', it will try to find the newest checkpoint
-- @cc@ from 'mghFrom' that's in main chain of responding node and
-- then return at most 'recoveryHeadersMessage' headers starting with
-- @cc@ as the oldest one, returning headers in range @l2 =
-- [cc.next..x]@ where @x@ is either @top@ (in case @length l2 <
-- recoveryHeadersMessage@) or some arbitrary header (and length is
-- precisely 'recoveryHeadersMessage').
data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: ![HeaderHash]
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetHeaders where
    build (MsgGetHeaders mghFrom mghTo) =
        bprint ("MsgGetHeaders {from = "%listJson%", to = "%build%"}")
               mghFrom (maybe "<Nothing>" (bprint build) mghTo)

-- | 'GetBlocks' message (see protocol specification).
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
newtype MsgBlock ssc =
    MsgBlock (Block ssc)
    deriving (Generic, Show)

deriving instance (Ssc ssc, Eq (SscPayload ssc)) => Eq (MsgBlock ssc)
