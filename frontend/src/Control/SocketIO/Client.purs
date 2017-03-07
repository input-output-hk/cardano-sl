module Control.SocketIO.Client where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3, runFn1, runFn3)

foreign import data SocketIO :: !
foreign import data Socket :: *

type Host = String
type Event = String

type EventHandler a eff = a -> Eff (socket :: SocketIO | eff) Unit

foreign import connectImpl :: forall eff. Host -> (Eff (socket :: SocketIO | eff) Socket)
foreign import emitImpl :: forall d eff. Fn3 Socket Event d (Eff (socket :: SocketIO | eff) Unit)
foreign import onImpl :: forall a eff. Fn3 Socket Event (EventHandler a eff) (Eff (socket :: SocketIO | eff) Unit)

connect :: forall eff. Host -> Eff (socket :: SocketIO | eff) Socket
connect host = runFn1 connectImpl host

emit :: forall d eff. Socket -> Event -> d -> Eff (socket :: SocketIO | eff) Unit
emit socket event dataObj = runFn3 emitImpl socket event dataObj

on :: forall a eff. Socket -> Event -> (EventHandler a eff) -> Eff (socket :: SocketIO | eff) Unit
on socket event callback = runFn3 onImpl socket event callback
