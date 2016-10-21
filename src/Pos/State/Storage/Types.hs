{-# LANGUAGE TemplateHaskell #-}

-- | Extra types used in Storage modules.

module Pos.State.Storage.Types
       ( AltChain
       , ProcessBlockRes (..)
       ) where

import           Data.List.NonEmpty   (NonEmpty)
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import           Serokell.Util.Verify (VerificationRes)
import           Universum

import           Pos.Types            (Block, HeaderHash)

-- | Alternative chain is a list of blocks which potentially
-- represents valid blockchain. Head of this list is the /oldest/ block.
type AltChain = NonEmpty Block

-- | Result of processNewBlock.
data ProcessBlockRes
    = -- | Block may be useful, but references unknown block. More
      -- blocks are needed to decide.
      PBRmore !HeaderHash
    | -- | Block has been adopted, head of main chain has been
      -- changed. Attached data is number of blocks to rollback and
      -- blocks which should be used instead.
      PBRgood !(Int, AltChain)
    | -- | Block has been discarded because of invalid data.
      PBRabort !VerificationRes

deriveSafeCopySimple 0 'base ''ProcessBlockRes
