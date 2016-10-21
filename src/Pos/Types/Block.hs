{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block related functions.

module Pos.Types.Block
       ( blockDifficulty

       , mkGenericHeader
       , mkGenericBlock
       , mkMainHeader
       , mkMainBlock

       -- , verifyGenericHeader
       , verifyHeader
       ) where

-- import           Data.Binary          (Binary)
-- import           Formatting           (build, sformat, (%))
-- import           Serokell.Util.Verify (VerificationRes (..), verifyGeneric)
-- import           Universum

-- import           Pos.Crypto           (Hash, PublicKey, SecretKey, hash, sign, toPublic,
--                                        unsafeHash, verify)
-- import           Pos.Types.Types      (BlockHeader, Blockchain (..), ChainDifficulty,
--                                        GenericBlock (..), GenericBlockHeader (..),
--                                        HeaderHash, MainBlock, MainBlockHeader,
--                                        MainBody (..), SlotId)
import           Pos.Types.Types (blockDifficulty, mkGenericBlock, mkGenericHeader,
                                  mkMainBlock, mkMainHeader, verifyHeader)
