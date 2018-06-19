{-# LANGUAGE CPP, GADTs, DeriveDataTypeable, TypeFamilies,
             FlexibleContexts, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Acid.Core
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  non-portable (uses GHC extensions)
--
-- Low-level controls for transaction-based state changes. This module defines
-- structures and tools for running state modifiers indexed either by an Method
-- or a serialized Method. This module should rarely be used directly although
-- the 'Method' class is needed when defining events manually.
--
-- The term \'Event\' is loosely used for transactions with ACID guarantees.
-- \'Method\' is loosely used for state operations without ACID guarantees
--
module Data.Acid.Core
    ( Core(coreMethods)
    , Method(..)
    , MethodContainer(..)
    , Tagged
    , mkCore
    , closeCore
    , closeCore'
    , modifyCoreState
    , modifyCoreState_
    , withCoreState
    , lookupHotMethod
    , lookupColdMethod
    , runHotMethod
    , runColdMethod
    , MethodMap
    , mkMethodMap
    ) where

import Control.Concurrent                 ( MVar, newMVar, withMVar
                                          , modifyMVar, modifyMVar_ )
import Control.Monad                      ( liftM )
import Control.Monad.State                ( State, runState )
import qualified Data.Map as Map
import Data.ByteString.Lazy as Lazy       ( ByteString )
import Data.ByteString.Lazy.Char8 as Lazy ( pack, unpack )

import Data.Serialize                     ( runPutLazy, runGetLazy )
import Data.SafeCopy                      ( SafeCopy, safeGet, safePut )

import Data.Typeable                      ( Typeable, TypeRep, typeRepTyCon, typeOf )
import Unsafe.Coerce                      ( unsafeCoerce )

#if MIN_VERSION_base(4,5,0)
import Data.Typeable                      ( tyConModule )
#else
import Data.Typeable.Internal             ( tyConModule )
#endif

#if MIN_VERSION_base(4,4,0)

-- in base >= 4.4 the Show instance for TypeRep no longer provides a
-- fully qualified name. But we have old data around that expects the
-- FQN. So we will recreate the old naming system for newer versions
-- of base. We could do something better, but happstack-state is
-- end-of-life anyway.
showQualifiedTypeRep :: TypeRep -> String
showQualifiedTypeRep tr = tyConModule con ++ "." ++ show tr
  where con = typeRepTyCon tr

#else

showQualifiedTypeRep :: TypeRep -> String
showQualifiedTypeRep tr = show tr

#endif

-- | The basic Method class. Each Method has an indexed result type
--   and a unique tag.
class ( Typeable ev, SafeCopy ev
      , Typeable (MethodResult ev), SafeCopy (MethodResult ev)) =>
      Method ev where
    type MethodResult ev
    type MethodState ev
    methodTag :: ev -> Tag
    methodTag ev = Lazy.pack (showQualifiedTypeRep (typeOf ev))

-- | The control structure at the very center of acid-state.
--   This module provides access to a mutable state through
--   methods. No efforts towards durability, checkpointing or
--   sharding happens at this level.
--   Important things to keep in mind in this module:
--     * We don't distinguish between updates and queries.
--     * We allow direct access to the core state as well
--       as through events.
data Core st
    = Core { coreState   :: MVar st
           , coreMethods :: MethodMap st
           }

-- | Construct a new Core using an initial state and a list of Methods.
mkCore :: [MethodContainer st]   -- ^ List of methods capable of modifying the state.
       -> st                     -- ^ Initial state value.
       -> IO (Core st)
mkCore methods initialValue
    = do mvar <- newMVar initialValue
         return Core{ coreState   = mvar
                    , coreMethods = mkMethodMap methods }

-- | Mark Core as closed. Any subsequent use will throw an exception.
closeCore :: Core st -> IO ()
closeCore core
    = closeCore' core (\_st -> return ())

-- | Access the state and then mark the Core as closed. Any subsequent use
--   will throw an exception.
closeCore' :: Core st -> (st -> IO ()) -> IO ()
closeCore' core action
    = modifyMVar_ (coreState core) $ \st ->
      do action st
         return errorMsg
    where errorMsg = error "Access failure: Core closed."

-- | Modify the state component. The resulting state is ensured to be in
--   WHNF.
modifyCoreState :: Core st -> (st -> IO (st, a)) -> IO a
modifyCoreState core action
    = modifyMVar (coreState core) $ \st -> do (!st', a) <- action st
                                              return (st', a)

-- | Modify the state component. The resulting state is ensured to be in
--   WHNF.
modifyCoreState_ :: Core st -> (st -> IO st) -> IO ()
modifyCoreState_ core action
    = modifyMVar_ (coreState core) $ \st -> do !st' <- action st
                                               return st'

-- | Access the state component.
withCoreState :: Core st -> (st -> IO a) -> IO a
withCoreState core = withMVar (coreState core)

-- | Execute a method as given by a type identifier and an encoded string.
--   The exact format of the encoded string depends on the type identifier.
--   Results are encoded and type tagged before they're handed back out.
--   This function is used when running events from a log-file or from another
--   server. Events that originate locally are most likely executed with
--   the faster 'runHotMethod'.
runColdMethod :: Core st -> Tagged Lazy.ByteString -> IO Lazy.ByteString
runColdMethod core taggedMethod
    = modifyCoreState core $ \st ->
      do let (a, st') = runState (lookupColdMethod core taggedMethod) st
         return ( st', a)

-- | Find the state action that corresponds to a tagged and serialized method.
lookupColdMethod :: Core st -> Tagged Lazy.ByteString -> State st Lazy.ByteString
lookupColdMethod core (storedMethodTag, methodContent)
    = case Map.lookup storedMethodTag (coreMethods core) of
        Nothing      -> missingMethod storedMethodTag
        Just (Method method)
          -> liftM (runPutLazy . safePut) (method (lazyDecode methodContent))

lazyDecode :: SafeCopy a => Lazy.ByteString -> a
lazyDecode inp
    = case runGetLazy safeGet inp of
        Left msg  -> error msg
        Right val -> val

missingMethod :: Tag -> a
missingMethod tag
    = error msg
    where msg = "This method is required but not available: " ++ show (Lazy.unpack tag) ++
                ". Did you perhaps remove it before creating a checkpoint?"

-- | Apply an in-memory method to the state.
runHotMethod :: Method method => Core (MethodState method) -> method -> IO (MethodResult method)
runHotMethod core method
    = modifyCoreState core $ \st ->
      do let (a, st') = runState (lookupHotMethod (coreMethods core) method) st
         return ( st', a)

-- | Find the state action that corresponds to an in-memory method.
lookupHotMethod :: Method method => MethodMap (MethodState method) -> method
                -> State (MethodState method) (MethodResult method)
lookupHotMethod methodMap method
    = case Map.lookup (methodTag method) methodMap of
        Nothing -> missingMethod (methodTag method)
        Just (Method methodHandler)
          -> -- If the methodTag doesn't index the right methodHandler then we're in deep
             -- trouble. Luckly, it would take deliberate malevolence for that to happen.
             unsafeCoerce methodHandler method

-- | Method tags must be unique and are most commonly generated automatically.
type Tag = Lazy.ByteString
type Tagged a = (Tag, a)

-- | Method container structure that hides the exact type of the method.
data MethodContainer st where
    Method :: (Method method) => (method -> State (MethodState method) (MethodResult method)) -> MethodContainer (MethodState method)

-- | Collection of Methods indexed by a Tag.
type MethodMap st = Map.Map Tag (MethodContainer st)

-- | Construct a 'MethodMap' from a list of Methods using their associated tag.
mkMethodMap :: [MethodContainer st] -> MethodMap st
mkMethodMap methods
    = Map.fromList [ (methodType method, method) | method <- methods ]
    where -- A little bit of ugliness is required to access the methodTags.
          methodType :: MethodContainer st -> Tag
          methodType m = case m of
                           Method fn -> let ev :: (ev -> State st res) -> ev
                                            ev _ = undefined
                                        in methodTag (ev fn)


