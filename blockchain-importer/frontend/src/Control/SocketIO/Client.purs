module Control.SocketIO.Client where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Data.Function.Uncurried (Fn2, Fn3, runFn1, runFn2, runFn3)

foreign import data SocketIO :: Effect
foreign import data Socket :: Type

type Host = String
type Event = String

type EventHandler a eff = a -> Eff (socket :: SocketIO | eff) Unit

foreign import connectImpl :: forall eff. Host -> (Eff (socket :: SocketIO | eff) Socket)
foreign import emitImpl :: forall eff. Fn2 Socket Event (Eff (socket :: SocketIO | eff) Unit)
foreign import emitDataImpl :: forall d eff. Fn3 Socket Event d (Eff (socket :: SocketIO | eff) Unit)
foreign import onImpl :: forall a eff. Fn3 Socket Event (EventHandler a eff) (Eff (socket :: SocketIO | eff) Unit)

connect :: forall eff. Host -> Eff (socket :: SocketIO | eff) Socket
connect host = runFn1 connectImpl host

-- | Emits an event without data
emit :: forall eff. Socket -> Event -> Eff (socket :: SocketIO | eff) Unit
emit socket event = runFn2 emitImpl socket event

-- | Emits an event and data
emitData :: forall d eff. Socket -> Event -> d -> Eff (socket :: SocketIO | eff) Unit
emitData socket event dataObj = runFn3 emitDataImpl socket event dataObj

on :: forall a eff. Socket -> Event -> (EventHandler a eff) -> Eff (socket :: SocketIO | eff) Unit
on socket event callback = runFn3 onImpl socket event callback
