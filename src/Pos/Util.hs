{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
       -- * Stuff for testing and benchmarking
         module Pos.Util.Arbitrary

       -- * Various
       , Raw
       , readerToState
       , eitherPanic
       , inAssertMode
       , diffDoubleMap
       , getKeys
       , maybeThrow
       , maybeThrow'

       -- * Lists
       , allDistinct

       -- * NonEmpty
       , NE
       , neZipWith3

       -- * Chronological sequences
       , NewestFirst(..)
       , OldestFirst(..)
       , toNewestFirst
       , toOldestFirst

       -- * SafeCopy
       , getCopyBinary
       , putCopyBinary

       -- * Lenses
       , makeLensesData
       , magnify'
       , _neHead
       , _neTail
       , _neLast
       , zoom'

       -- * Prettification
       , Color (..)
       , colorize
       , withColoredMessages

       -- * TimeWarp helpers
       , CanLogInParallel
       , WaitingDelta (..)
       , logWarningLongAction
       , logWarningWaitOnce
       , logWarningWaitLinear
       , logWarningWaitInf
       , runWithRandomIntervals'
       , waitRandomInterval'
       , runWithRandomIntervals
       , runWithRandomIntervalsNow
       , waitRandomInterval

       -- * LRU
       , clearLRU

       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM

       , spanSafe
       , eitherToVerRes

       -- * MVar
       , clearMVar
       , forcePutMVar
       , readMVarConditional
       , readUntilEqualMVar
       , readTVarConditional
       , readUntilEqualTVar

       , stubListenerOneMsg
       , stubListenerConv

       , withWaitLogConv
       , withWaitLogConvL
       , withWaitLog

       , execWithTimeLimit

       , NamedMessagePart (..)
       -- * Instances
       -- ** SafeCopy (NonEmpty a)
       -- ** MonadFail (Either s), assuming IsString s
       -- ** MonadFail ParsecT
       -- ** MonadFail Dialog
       -- ** MonadFail Transfer
       -- ** MonadFail TimedIO
       -- ** MonadFail ResponseT
       -- ** MonadFail LoggerNameBox
       ) where

import           Control.Concurrent.STM.TVar   (TVar, readTVar)
import           Control.Lens                  (Each (..), LensLike', Magnified, Zoomed,
                                                lensRules, magnify, makeWrapped, zoom,
                                                _Wrapped)
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Control.Monad                 as Monad (fail)
import           Control.Monad.STM             (retry)
import           Control.Monad.Trans.Resource  (ResourceT)
import           Data.Binary                   (Binary)
import qualified Data.Cache.LRU                as LRU
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.HashSet                  (fromMap)
import           Data.List                     (span, zipWith3)
import qualified Data.List.NonEmpty            as NE
import           Data.Proxy                    (Proxy (..), asProxyTypeOf)
import           Data.SafeCopy                 (Contained, SafeCopy (..), base, contain,
                                                deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)
import qualified Data.Text                     as T
import           Data.Time.Units               (Microsecond, Second, convertUnit)
import           Formatting                    (sformat, shown, stext, (%))
import           Language.Haskell.TH
import           Mockable                      (Async, Bracket, Delay, Fork, Mockable,
                                                Promise, Throw, async, bracket, cancel,
                                                delay, fork, killThread, throw, waitAny)
import           Node                          (ConversationActions (..),
                                                ListenerAction (..), Message, NodeId,
                                                SendActions (..))
import           Node.Message                  (MessageName (..), Packable, Unpackable,
                                                messageName, messageName')
import           Serokell.Util                 (VerificationRes (..))
import           System.Console.ANSI           (Color (..), ColorIntensity (Vivid),
                                                ConsoleLayer (Foreground),
                                                SGR (Reset, SetColor), setSGRCode)
import           System.Wlog                   (LoggerNameBox (..), WithLogger, logDebug,
                                                logWarning, modifyLoggerName)
import           Test.QuickCheck               (Arbitrary)
import           Text.Parsec                   (ParsecT)
import           Universum                     hiding (Async, async, bracket, cancel,
                                                waitAny)
import           Unsafe                        (unsafeInit, unsafeLast)

-- SafeCopy instance for HashMap
import           Serokell.AcidState.Instances  ()

import           Pos.Binary.Class              (Bi)
import qualified Pos.Binary.Class              as Bi
import           Pos.Crypto.Random             (randomNumber)
import           Pos.Util.Arbitrary
import           Pos.Util.NotImplemented       ()

-- | Helper class used for Pos.Util.Relay
class NamedMessagePart a where
    nMessageName :: Proxy a -> Text

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable)

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Binary'
-- instance.
putCopyBinary :: Bi a => a -> Contained Cereal.Put
putCopyBinary x = contain $ safePut (Bi.encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Binary'
-- instance.
getCopyBinary :: Bi a => String -> Contained (Cereal.Get a)
getCopyBinary typeName = contain $ do
    bs <- safeGet
    case Bi.decodeFull bs of
        Left err -> fail ("getCopy@" ++ typeName ++ ": " ++ err)
        Right x  -> return x

-- | Convert (Reader s) to any (MonadState s)
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

deriveSafeCopySimple 0 'base ''VerificationRes

-- | A helper for simple error handling in executables
eitherPanic :: Show a => Text -> Either a b -> b
eitherPanic msgPrefix = either (panic . (msgPrefix <>) . show) identity

-- | This function performs checks at compile-time for different actions.
-- May slowdown implementation. To disable such checks (especially in benchmarks)
-- one should compile with: @stack build --flag cardano-sl:-asserts@
inAssertMode :: Applicative m => m a -> m ()
#ifdef ASSERTS_ON
inAssertMode x = x *> pure ()
#else
inAssertMode _ = pure ()
#endif
{-# INLINE inAssertMode #-}

-- | Remove elements which are in 'b' from 'a'
diffDoubleMap
    :: forall k1 k2 v.
       (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.foldlWithKey' go mempty a
  where
    go :: HashMap k1 (HashMap k2 v)
       -> k1
       -> HashMap k2 v
       -> HashMap k1 (HashMap k2 v)
    go res extKey internalMap =
        case HM.lookup extKey b of
            Nothing -> HM.insert extKey internalMap res
            Just internalMapB ->
                let diff = internalMap `HM.difference` internalMapB
                in if null diff
                       then res
                       else HM.insert extKey diff res

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

maybeThrow' :: (Mockable Throw m, Exception e) => e -> Maybe a -> m a
maybeThrow' e = maybe (throw e) pure

----------------------------------------------------------------------------
-- List utils
----------------------------------------------------------------------------

allDistinct :: Ord a => [a] -> Bool
allDistinct xs = and $ zipWith (/=) sorted (drop 1 sorted)
  where
    sorted = sort xs

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

type NE = NonEmpty

neZipWith3 :: (x -> y -> z -> q) -> NonEmpty x -> NonEmpty y -> NonEmpty z -> NonEmpty q
neZipWith3 f (x :| xs) (y :| ys) (z :| zs) = f x y z :| zipWith3 f xs ys zs

----------------------------------------------------------------------------
-- Chronological sequences
----------------------------------------------------------------------------

newtype NewestFirst f a = NewestFirst {getNewestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)
newtype OldestFirst f a = OldestFirst {getOldestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)

makeWrapped ''NewestFirst
makeWrapped ''OldestFirst

instance Each (f a) (f b) a b =>
         Each (NewestFirst f a) (NewestFirst f b) a b where
    each = _Wrapped . each
instance Each (f a) (f b) a b =>
         Each (OldestFirst f a) (OldestFirst f b) a b where
    each = _Wrapped . each

instance One (f a) => One (NewestFirst f a) where
    type OneItem (NewestFirst f a) = OneItem (f a)
    one = NewestFirst . one
instance One (f a) => One (OldestFirst f a) where
    type OneItem (OldestFirst f a) = OneItem (f a)
    one = OldestFirst . one

class Chrono f where
    toNewestFirst :: OldestFirst f a -> NewestFirst f a
    toOldestFirst :: NewestFirst f a -> OldestFirst f a

instance Chrono [] where
    toNewestFirst = NewestFirst . reverse . getOldestFirst
    toOldestFirst = OldestFirst . reverse . getNewestFirst

instance Chrono NonEmpty where
    toNewestFirst = NewestFirst . NE.reverse . getOldestFirst
    toOldestFirst = OldestFirst . NE.reverse . getNewestFirst

----------------------------------------------------------------------------
-- Lens utils
----------------------------------------------------------------------------

-- | Make lenses for a data family instance.
makeLensesData :: Name -> Name -> DecsQ
makeLensesData familyName typeParamName = do
    info <- reify familyName
    ins <- case info of
        FamilyI _ ins -> return ins
        _             -> fail "makeLensesIndexed: expected data family name"
    typeParamInfo <- reify typeParamName
    typeParam <- case typeParamInfo of
        TyConI dec -> decToType dec
        _          -> fail "makeLensesIndexed: expected a type"
    let mbInsDec = find ((== Just typeParam) . getTypeParam) ins
    case mbInsDec of
        Nothing -> fail ("makeLensesIndexed: an instance for " ++
                         nameBase typeParamName ++ " not found")
        Just insDec -> makeFieldOpticsForDec lensRules insDec
  where
    getTypeParam (NewtypeInstD _ _ [t] _ _ _) = Just t
    getTypeParam (DataInstD    _ _ [t] _ _ _) = Just t
    getTypeParam _                            = Nothing

    decToType (DataD    _ n _ _ _ _) = return (ConT n)
    decToType (NewtypeD _ n _ _ _ _) = return (ConT n)
    decToType other                  =
        fail ("makeLensesIndexed: decToType failed on: " ++ show other)

-- | Lens for the head of 'NonEmpty'.
--
-- We can't use '_head' because it doesn't work for 'NonEmpty':
-- <https://github.com/ekmett/lens/issues/636#issuecomment-213981096>.
-- Even if we could though, it wouldn't be a lens, only a traversal.
_neHead :: Lens' (NonEmpty a) a
_neHead f (x :| xs) = (:| xs) <$> f x

-- | Lens for the tail of 'NonEmpty'.
_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

-- | Lens for the last element of 'NonEmpty'.
_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| unsafeInit xs ++ [y]) <$> f (unsafeLast xs)

-- [SRK-51]: we should try to get this one into safecopy itself though it's
-- unlikely that they will choose a different implementation (if they do
-- choose a different implementation we'll have to write a migration)
--
-- update: made a PR <https://github.com/acid-state/safecopy/pull/47>;
-- remove this instance when the pull request is merged
instance SafeCopy a => SafeCopy (NonEmpty a) where
    getCopy = contain $ do
        xs <- safeGet
        case nonEmpty xs of
            Nothing -> fail "getCopy@NonEmpty: list can't be empty"
            Just xx -> return xx
    putCopy = contain . safePut . toList
    errorTypeName _ = "NonEmpty"

-- | A 'zoom' which works in 'MonadState'.
--
-- See <https://github.com/ekmett/lens/issues/580>. You might be surprised
-- but actual 'zoom' doesn't work in any 'MonadState', it only works in a
-- handful of state monads and their combinations defined by 'Zoom'.
zoom'
    :: MonadState s m
    => LensLike' (Zoomed (State s) a) s t -> StateT t Identity a -> m a
zoom' l = state . runState . zoom l

-- | A 'magnify' which in 'MonadReader'.
magnify'
    :: MonadReader s m
    => LensLike' (Magnified (Reader s) a) s t -> ReaderT t Identity a -> m a
magnify' l = reader . runReader . magnify l

-- Monad z => Zoom (StateT s z) (StateT t z) s t
-- Monad z => Zoom (StateT s z) (StateT t z) s t

----------------------------------------------------------------------------
-- Prettification.
----------------------------------------------------------------------------

-- | Prettify 'Text' message with 'Vivid' color.
colorize :: Color -> Text -> Text
colorize color msg =
    mconcat
        [ toText (setSGRCode [SetColor Foreground Vivid color])
        , msg
        , toText (setSGRCode [Reset])
        ]

-- | Write colored message, do some action, write colored message.
-- Intended for debug only.
withColoredMessages :: MonadIO m => Color -> Text -> m a -> m a
withColoredMessages color activity action = do
    putText (colorize color $ sformat ("Entered "%stext%"\n") activity)
    res <- action
    res <$ putText (colorize color $ sformat ("Finished "%stext%"\n") activity)

-- | Data type to represent waiting strategy for printing warnings
-- if action take too much time.
--
-- [LW-4]: this probably will be moved somewhere from here
data WaitingDelta
    = WaitOnce      Second              -- ^ wait s seconds and stop execution
    | WaitLinear    Second              -- ^ wait s, s * 2, s * 3  , s * 4  , ...      seconds
    | WaitGeometric Microsecond Double  -- ^ wait m, m * q, m * q^2, m * q^3, ... microseconds
    deriving (Show)

-- | Constraint for something that can be logged in parallel with other action.
type CanLogInParallel m = (Mockable Delay m, Mockable Fork m, WithLogger m, Mockable Bracket m)

-- | Run action and print warning if it takes more time than expected.
logWarningLongAction :: CanLogInParallel m => WaitingDelta -> Text -> m a -> m a
logWarningLongAction delta actionTag action =
    bracket (fork $ waitAndWarn delta) onFinish (const action)
  where
    onFinish logThreadId = do
        killThread logThreadId
        --logDebug (sformat ("Action `"%stext%"` finished") actionTag)
    printWarning t = logWarning $ sformat ("Action `"%stext%"` took more than "%shown)
                                  actionTag
                                  t

    -- [LW-4]: avoid code duplication somehow (during refactoring)
    waitAndWarn (WaitOnce      s  ) = delay s >> printWarning s
    waitAndWarn (WaitLinear    s  ) = let waitLoop acc = do
                                              delay s
                                              printWarning acc
                                              waitLoop (acc + s)
                                      in waitLoop s
    waitAndWarn (WaitGeometric s q) = let waitLoop acc t = do
                                              delay t
                                              let newAcc = acc + t
                                              let newT   = round $ fromIntegral t * q
                                              printWarning (convertUnit newAcc :: Second)
                                              waitLoop newAcc newT
                                      in waitLoop 0 s

{- Helper functions to avoid dealing with data type -}

-- | Specialization of 'logWarningLongAction' with 'WaitOnce'.
logWarningWaitOnce :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitOnce = logWarningLongAction . WaitOnce

-- | Specialization of 'logWarningLongAction' with 'WaiLinear'.
logWarningWaitLinear :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitLinear = logWarningLongAction . WaitLinear

-- | Specialization of 'logWarningLongAction' with 'WaitGeometric'
-- with parameter @1.3@. Accepts 'Second'.
logWarningWaitInf :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitInf = logWarningLongAction . (`WaitGeometric` 1.3) . convertUnit

-- | Wait random number of 'Microsecond'`s between min and max.
waitRandomInterval
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m ()
waitRandomInterval minT maxT = do
    interval <-
        (+ minT) . fromIntegral <$>
        liftIO (randomNumber $ fromIntegral $ maxT - minT)
    delay interval

-- | Wait random interval and then perform given action.
runWithRandomIntervals
    :: (MonadIO m, WithLogger m, Mockable Fork m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervals minT maxT action = do
  waitRandomInterval minT maxT
  action
  runWithRandomIntervals minT maxT action

-- | Like `runWithRandomIntervals`, but performs action immidiatelly
-- at first time.
runWithRandomIntervalsNow
    :: (MonadIO m, WithLogger m, Mockable Fork m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervalsNow minT maxT action = do
  action
  runWithRandomIntervals minT maxT action

-- TODO remove MonadIO in preference to some `Mockable Random`
-- | Wait random number of 'Microsecond'`s between min and max.
waitRandomInterval'
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m ()
waitRandomInterval' minT maxT = do
    interval <-
        (+ minT) . fromIntegral <$>
        liftIO (randomNumber $ fromIntegral $ maxT - minT)
    delay interval

-- | Wait random interval and then perform given action.
runWithRandomIntervals'
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervals' minT maxT action = do
  waitRandomInterval' minT maxT
  action
  runWithRandomIntervals' minT maxT action

----------------------------------------------------------------------------
-- LRU cache
----------------------------------------------------------------------------

-- | Remove all items from LRU, retaining maxSize property.
clearLRU :: Ord k => LRU.LRU k v -> LRU.LRU k v
clearLRU = LRU.newLRU . LRU.maxSize

-- [SRK-51]: Probably this instance should be in @safecopy@ library
instance (Ord k, SafeCopy k, SafeCopy v) =>
         SafeCopy (LRU.LRU k v) where
    getCopy = contain $ LRU.fromList <$> safeGet <*> safeGet
    putCopy lru =
        contain $
        do safePut $ LRU.maxSize lru
           safePut $ LRU.toList lru
    errorTypeName _ = "LRU"

-- | Create HashSet from HashMap's keys
getKeys :: HashMap k v -> HashSet k
getKeys = fromMap . void

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

-- | See `Pos.Crypto.SerTypes` for details on this types

newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

class AsBinaryClass a where
  asBinary :: a -> AsBinary a
  fromBinary :: AsBinary a -> Either String a

fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either fail return . fromBinary

eitherToVerRes :: Either Text a -> VerificationRes
eitherToVerRes (Left errors) = if T.null errors then VerFailure []
                               else VerFailure $ T.split (==';') errors
eitherToVerRes (Right _ )    = VerSuccess

-- | Makes a span on the list, considering tail only. Predicate has
-- list head as first argument. Used to take non-null prefix that
-- depends on the first element.
spanSafe :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanSafe p (x:|xs) = let (a,b) = span (p x) xs in (x:|a,b)

instance IsString s => MonadFail (Either s) where
    fail = Left . fromString

instance MonadFail (ParsecT s u m) where
    fail = Monad.fail

deriving instance MonadFail m => MonadFail (LoggerNameBox m)

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . fail

----------------------------------------------------------------------------
-- MVar utilities
----------------------------------------------------------------------------

clearMVar :: MonadIO m => MVar a -> m ()
clearMVar = liftIO . void . tryTakeMVar

forcePutMVar :: MonadIO m => MVar a -> a -> m ()
forcePutMVar mvar val = do
    res <- liftIO $ tryPutMVar mvar val
    unless res $ do
        _ <- liftIO $ tryTakeMVar mvar
        forcePutMVar mvar val

-- | Block until value in MVar satisfies given predicate. When value
-- satisfies, it is returned.
readMVarConditional :: (MonadIO m) => (x -> Bool) -> MVar x -> m x
readMVarConditional predicate mvar = do
    rData <- liftIO . readMVar $ mvar -- first we try to read for optimization only
    if predicate rData then pure rData
    else do
        tData <- liftIO . takeMVar $ mvar -- now take data
        if predicate tData then do -- check again
            _ <- liftIO $ tryPutMVar mvar tData -- try to put taken value
            pure tData
        else
            readMVarConditional predicate mvar

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualMVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> MVar x -> a -> m x
readUntilEqualMVar f mvar expVal = readMVarConditional ((expVal ==) . f) mvar

-- | Block until value in TVar satisfies given predicate. When value
-- satisfies, it is returned.
readTVarConditional :: (MonadIO m) => (x -> Bool) -> TVar x -> m x
readTVarConditional predicate tvar = atomically $ do
    res <- readTVar tvar
    if predicate res then pure res
    else retry

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualTVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> TVar x -> a -> m x
readUntilEqualTVar f tvar expVal = readTVarConditional ((expVal ==) . f) tvar

stubListenerOneMsg
    :: (WithLogger m, Message r, Unpackable p r, Packable p r)
    => Proxy r -> ListenerAction p m
stubListenerOneMsg p = ListenerActionOneMsg $ \_ _ m ->
                          let _ = m `asProxyTypeOf` p
                           in modifyLoggerName (<> "stub") $
                                logDebug $ sformat
                                    ("Stub listener (one msg) for "%shown%": received message")
                                    (messageName p)

stubListenerConv
    :: (WithLogger m, Message r, Unpackable p r, Packable p Void)
    => Proxy r -> ListenerAction p m
stubListenerConv p = ListenerActionConversation $ \__nId convActions ->
                          let _ = convActions `asProxyTypeOf` __modP p
                              __modP :: Proxy r -> Proxy (ConversationActions Void r m)
                              __modP _ = Proxy
                           in modifyLoggerName (<> "stub") $
                                logDebug $ sformat
                                    ("Stub listener (conv) for "%shown%": received message")
                                    (messageName p)

withWaitLog :: ( CanLogInParallel m ) => SendActions p m -> SendActions p m
withWaitLog sendActions = sendActions
    { sendTo = \nodeId msg ->
                  let MessageName mName = messageName' msg
                   in logWarningWaitLinear 4
                        (sformat ("Send "%shown%" to "%shown) mName nodeId) $
                          sendTo sendActions nodeId msg
    , withConnectionTo = \nodeId action -> withConnectionTo sendActions nodeId $ action . withWaitLogConv nodeId
    }

withWaitLogConv
    :: (CanLogInParallel m, Message snd)
    => NodeId -> ConversationActions snd rcv m -> ConversationActions snd rcv m
withWaitLogConv nodeId conv = conv { send = send', recv = recv' }
  where
    send' msg =
        logWarningWaitLinear 4
          (sformat ("Send "%shown%" to "%shown%" in conversation") sndMsg nodeId) $
            send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv from "%shown%" in conversation") nodeId) $
            recv conv
    MessageName sndMsg = messageName $ ((\_ -> Proxy) :: ConversationActions snd rcv m -> Proxy snd) conv

withWaitLogConvL
    :: (CanLogInParallel m, Message rcv)
    => NodeId -> ConversationActions snd rcv m -> ConversationActions snd rcv m
withWaitLogConvL nodeId conv = conv { send = send', recv = recv' }
  where
    send' msg =
        logWarningWaitLinear 4
          (sformat ("Send to "%shown%" in conversation") nodeId) $
            send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv "%shown%" from "%shown%" in conversation") rcvMsg nodeId) $
            recv conv
    MessageName rcvMsg = messageName $ ((\_ -> Proxy) :: ConversationActions snd rcv m -> Proxy rcv) conv



execWithTimeLimit :: ( Mockable Async m
                     , Mockable Delay m
                     , Eq (Promise m (Maybe a))
                     ) => Microsecond -> m a -> m (Maybe a)
execWithTimeLimit timeout action = do
    promises <- mapM async [ Just <$> action, delay timeout $> Nothing ]
    (promise, val) <- waitAny promises
    mapM_ cancel $ filter (/= promise) promises
    return val
