{-# LANGUAGE TemplateHaskell #-}

-- | Extra types used in Storage modules.

module Pos.State.Storage.Types
       ( AltChain
       , ProcessBlockRes (..)
       , mkPBRabort
       , ProcessTxRes (..)
       , mkPTRinvalid
       ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.SafeCopy      (base, deriveSafeCopySimple)
import qualified Data.Text          as T
import           Universum

import           Pos.Types          (Block, HeaderHash)

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
      PBRgood !(Word, AltChain)
    | -- | Block has been discarded because it's not interesting or invalid.
      PBRabort !Text
    deriving (Show)

-- | Make `ProcessBlockRes` from list of error messages using
-- `PBRabort` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkPBRabort :: [Text] -> ProcessBlockRes
mkPBRabort = PBRabort . T.intercalate "; "

deriveSafeCopySimple 0 'base ''ProcessBlockRes

data ProcessTxRes
    = PTRadded
    | PTRknown
    | PTRinvalid !Text
    deriving (Show)

deriveSafeCopySimple 0 'base ''ProcessTxRes

-- | Similar to mkPBRabort
mkPTRinvalid :: [Text] -> ProcessTxRes
mkPTRinvalid = PTRinvalid . T.intercalate "; "
