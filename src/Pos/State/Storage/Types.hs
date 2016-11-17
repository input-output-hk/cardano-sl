{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Extra types used in Storage modules.

module Pos.State.Storage.Types
       ( AltChain
       , ProcessBlockRes (..)
       , mkPBRabort
       , ProcessTxRes (..)
       , mkPTRinvalid
       ) where

import           Control.Monad.Fail  (fail)
import           Data.List.NonEmpty  (NonEmpty)
import           Data.SafeCopy       (SafeCopy (..), base, contain, deriveSafeCopySimple,
                                      safeGet, safePut)
import qualified Data.Serialize      as Cereal (getWord8, putWord8)
import qualified Data.Text           as T
import           Universum

import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, BodyProof, HeaderHash, MainBlockchain)

-- | Alternative chain is a list of blocks which potentially
-- represents valid blockchain. Head of this list is the /oldest/ block.
type AltChain ssc = NonEmpty (Block ssc)

-- | Result of processNewBlock.
data ProcessBlockRes ssc
    = -- | Block may be useful, but references unknown block. More
      -- blocks are needed to decide.
      PBRmore !(HeaderHash ssc)
    | -- | Block has been adopted, head of main chain has been
      -- changed. Attached data is number of blocks to rollback and
      -- blocks which should be used instead.
      PBRgood !(Word, AltChain ssc)
    | -- | Block has been discarded because it's not interesting or invalid.
      PBRabort !Text

deriving instance
         (Ssc ssc, Show (BodyProof (MainBlockchain ssc))) =>
         Show (ProcessBlockRes ssc)

-- | Make `ProcessBlockRes` from list of error messages using
-- `PBRabort` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkPBRabort :: [Text] -> ProcessBlockRes ssc
mkPBRabort = PBRabort . T.intercalate "; "

instance Ssc ssc => SafeCopy (ProcessBlockRes ssc) where
    getCopy =
        contain $
        do t <- Cereal.getWord8
           case t of
               0 -> PBRmore <$> safeGet
               1 -> PBRgood <$> safeGet
               2 -> PBRabort <$> safeGet
               _ -> fail "getCopy@ProcessBlockRes: unknown tag"
    putCopy x =
        contain $
        do case x of
               PBRmore a  -> Cereal.putWord8 0 >> safePut a
               PBRgood a  -> Cereal.putWord8 1 >> safePut a
               PBRabort a -> Cereal.putWord8 2 >> safePut a

data ProcessTxRes
    = PTRadded
    | PTRknown
    | PTRinvalid !Text
    | PTRoverwhelmed
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''ProcessTxRes

-- | Similar to mkPBRabort
mkPTRinvalid :: [Text] -> ProcessTxRes
mkPTRinvalid = PTRinvalid . T.intercalate "; "
