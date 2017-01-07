{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.NewDHT.Model.Class.BiP
       (
         BiP(..)
       ) where

import           Control.Monad.Catch               (MonadThrow (..))
import           Data.Binary.Get                   (Get, isEmpty, label, runGetOrFail)
import           Data.Binary.Put                   (execPut)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder.Extra     as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit                      ((=$=))
import qualified Data.Conduit.List                 as CL
import           Data.Conduit.Serialization.Binary (ParseError (..), conduitGet,
                                                    conduitPut)
import           Message.Message                   (Packable (..), Unpackable (..),
                                                    recvNext)
import           Universum                         hiding (yield)

import           Pos.Binary.Class                  (Bi (..))

data BiP = BiP

instance Bi r => Packable BiP r where
    packMsg _ m = BS.toLazyByteStringWith
                    (BS.untrimmedStrategy 256 4096)
                    LBS.empty
                  . execPut
                  $ put m

instance Bi r => Unpackable BiP r where
    unpackMsg _ = recvNext get
