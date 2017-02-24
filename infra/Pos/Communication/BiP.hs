{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       ) where

import           Data.Binary.Get               (runGetIncremental)
import           Data.Binary.Put               (execPut)
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Node.Message                  (Packable (..), Unpackable (..))
import           Universum                     hiding (yield)

import           Pos.Binary.Class              (Bi (..))

data BiP = BiP

instance Bi r => Packable BiP r where
    packMsg _ m = BS.toLazyByteStringWith
                    (BS.untrimmedStrategy 256 4096)
                    LBS.empty
                  . execPut
                  $ put m

instance Bi r => Unpackable BiP r where
    unpackMsg _ = runGetIncremental get
