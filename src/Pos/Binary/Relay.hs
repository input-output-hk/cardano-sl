-- | Pos.Util.Relay serialization instances

module Pos.Binary.Relay () where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (getWord8)
import           Data.Binary.Put    (putWord8)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Binary.Crypto  ()
import           Pos.Util.Relay     (DataMsg (..), InvMsg (..), ReqMsg (..))

instance Bi tag => Bi (InvMsg tag) where
  put InvMsg {..} = put imTag >> put imStakeholders
  get = liftM2 InvMsg get get

instance Bi tag => Bi (ReqMsg tag) where
  put ReqMsg {..} = put rmTag >> put rmStakeholders
  get = liftM2 ReqMsg get get

instance Bi contents => Bi (DataMsg contents) where
  put DataMsg {..} = put dmContents >> put dmStakeholder
  get = liftM2 DataMsg get get
