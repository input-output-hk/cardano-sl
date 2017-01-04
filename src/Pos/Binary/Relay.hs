-- | Pos.Util.Relay serialization instances

module Pos.Binary.Relay () where

import           Control.Monad.Fail               (fail)
import           Data.Binary.Get                  (getWord8)
import           Data.Binary.Put                  (putWord8)
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Crypto                       (hash)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Txp.Types.Communication      (TxMsgContents (..), TxMsgTag (..))
import           Pos.Types                        (TxId)
import           Pos.Types.Address                (StakeholderId)
import           Pos.Util.Relay                   (DataMsg (..), InvMsg (..), ReqMsg (..))

instance (Bi tag, Bi key) => Bi (InvMsg key tag) where
    put InvMsg {..} = put imTag >> put imKeys
    get = liftM2 InvMsg get get

instance (Bi tag, Bi key) => Bi (ReqMsg key tag) where
    put ReqMsg {..} = put rmTag >> put rmKeys
    get = liftM2 ReqMsg get get

-- Sometimes we want another instances to exist
--instance (Bi tag, Bi contents) => Bi (DataMsg key contents) where
--    put DataMsg {..} = put dmContents >> put dmKey
--    get = liftM2 DataMsg get get

instance Bi (DataMsg StakeholderId GtMsgContents) where
    put DataMsg {..} = put dmContents >> put dmKey
    get = liftM2 DataMsg get get

instance Bi (DataMsg TxId TxMsgContents) where
    put (DataMsg (TxMsgContents dmTx dmWitness dmDistr) _) =
        put dmTx >> put dmWitness >> put dmDistr
    get = do
      tx <- get
      conts <- TxMsgContents tx <$> get <*> get
      pure $ DataMsg conts (hash tx)
