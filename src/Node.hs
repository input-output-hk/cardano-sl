{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Node where

import Control.Monad.Fix (MonadFix)
import qualified Node.Internal as LL
import Node.Internal (ChannelIn(..), ChannelOut(..))
import Data.String (fromString, IsString)
import Data.Binary     as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Network.Transport.Abstract as NT
import System.Random (StdGen)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Channel
import Mockable.SharedAtomic
import Mockable.Exception
import GHC.Generics (Generic)

data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

type NodeId = LL.NodeId

nodeId :: Node m -> NodeId
nodeId = LL.NodeId . NT.address . LL.nodeEndPoint . nodeLL

type Worker m = SendActions m -> m ()

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
instance Binary MessageName

data Listener m = Listener MessageName (ListenerAction m)

data ListenerAction m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: Binary msg
    => (NodeId -> SendActions m -> msg -> m ())
    -> ListenerAction m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: (NodeId -> ConversationActions m -> m ())
    -> ListenerAction m

data SendActions m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body. Binary body => NodeId -> MessageName -> body -> m (),

       -- | Establish a bi-direction conversation session with a node
       connect :: NodeId -> m (ConversationActions m)
     }

data ConversationActions m = ConversationActions {
       -- | Send a message within the context of this conversation
       send  :: forall msg. Binary msg => msg -> m (),

       -- | Receive a message within the context of this conversation
       recv  :: forall msg. Binary msg => m msg,

       -- | Close the outbound side of this conversation
       close :: m ()  -- TODO: needed? if so only for early close. Should by automatic.
     }

type ListenerIndex m = Map MessageName (ListenerAction m)

makeListenerIndex :: [Listener m] -> (ListenerIndex m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine (Listener name action) (map, existing) =
        let (replaced, map') = M.insertLookupWithKey (\_ _ _ -> action) name action map
            overlapping = maybe [] (const [name]) replaced
        in  (map', overlapping ++ existing)

startNode
    :: forall m .
       ( Mockable Fork m, Mockable RunInUnboundThread m, Mockable Throw m
       , Mockable Channel m, Mockable SharedAtomic m, MonadFix m )
    => NT.Transport m
    -> StdGen
    -> [Worker m]
    -> [Listener m]
    -> m (Node m)
startNode transport prng workers listeners = mdo
    node <- LL.startNode transport prng (handlerIn node sendAction) (handlerInOut node)
    let sendAction = SendActions { sendTo = sendMsg node, connect = undefined }
    tids <- sequence
              [ fork $ worker sendAction
              | worker <- workers ]
    return Node {
      nodeLL      = node,
      nodeWorkers = tids
    }
  where
    -- Index the listeners by message name, for faster lookup.
    listenerIndex :: ListenerIndex m
    (listenerIndex, conflictingNames) = makeListenerIndex listeners

    handlerIn :: LL.Node m -> SendActions m -> NodeId -> ChannelIn m -> m ()
    handlerIn node sendActions peerId (ChannelIn chan) = do
        (msgName :: MessageName, rest) <- recvPart chan (fromString "")
        let listener = M.lookup msgName listenerIndex
        case listener of
            Just (ListenerActionOneMsg action) -> do
                (msgBody, rest') <- recvPart chan rest
                tid <- fork (action peerId sendActions msgBody)
                -- TODO remember the thread id? Inform dispatcher when it's
                -- finished?
                pure ()
            -- If it's a conversation listener, then that's an error, no?
            Just (ListenerActionConversation _) -> error ("Wrong listener type! " ++ show msgName)
            Nothing -> error ("No listener! " ++ show msgName)
        pure ()

    handlerInOut :: LL.Node m -> NodeId -> ChannelIn m -> ChannelOut m -> m ()
    handlerInOut node peerId (ChannelIn inchan) (ChannelOut outchan) = do
        (msgName :: MessageName, rest) <- recvPart inchan (fromString "")
        let listener = M.lookup msgName listenerIndex
        case listener of
            Just (ListenerActionConversation action) -> do
                -- NB we do not pull the body from the channel.
                -- It's up to the listener to do so.
                -- We also need a channel which will accept messages without
                -- names.
                let csend :: Binary body => body -> m ()
                    -- Write to the outchan.
                    csend = undefined
                let crecv :: Binary body => m body
                    -- Read from the inchan.
                    crecv = undefined
                let cclose :: m ()
                    -- I dunno.
                    cclose = undefined
                let conversationActions = ConversationActions csend crecv cclose
                tid <- fork (action peerId conversationActions)
                pure ()
            Just (ListenerActionOneMsg _) -> error ("Wrong listener type! " ++ show msgName)
            Nothing -> error ("No listener! " ++ show msgName)
        pure ()

    --TODO: fill in the dispatcher impl:

stopNode :: ( Mockable Fork m ) => Node m -> m ()
stopNode Node {..} = do
    LL.stopNode nodeLL
    -- Stop the workers
    mapM_ killThread nodeWorkers
    -- alternatively we could try stopping new incoming messages
    -- and wait for all handlers to finish

sendMsg
    :: ( Monad m, Binary body )
    => LL.Node m
    -> NodeId
    -> MessageName
    -> body
    -> m ()
sendMsg node nodeid name body =
    LL.sendMsg node nodeid (serialiseMsg name body)

serialiseMsg :: Binary body => MessageName -> body -> LBS.ByteString
serialiseMsg name body =
    serialise $ do
      Bin.put name
      Bin.put body
  where
    serialise = BS.toLazyByteStringWith
                 (BS.untrimmedStrategy 256 4096)
                 LBS.empty
              . Bin.execPut

sendBody
    :: ( Monad m, Binary body )
    => LL.Node m
    -> NodeId
    -> body
    -> m ()
sendBody node nodeid body =
    LL.sendMsg node nodeid (serialiseBody body)

serialiseBody
    :: ( Binary body )
    => body
    -> LBS.ByteString
serialiseBody =
    BS.toLazyByteStringWith (BS.untrimmedStrategy 256 4096) LBS.empty
    . Bin.execPut
    . put

recvPart
    :: ( Mockable Channel m, Binary thing )
    => ChannelT m (Maybe BS.ByteString)    -- source
    -> BS.ByteString                       -- prefix
    -> m (thing, BS.ByteString)            -- trailing
recvPart chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental Bin.get) prefix)
  where
    go (Bin.Done trailing _ a) = return (a, trailing)
    go (Bin.Fail _trailing _ err) = fail "TODO"
    go (Bin.Partial continue) = do
      mx <- readChannel chan
      go (continue mx)
