{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Pos.Util
       (
       -- * Stuff for testing and benchmarking
         module UtilArbitrary

       -- * Various
       , Raw
       , readerToState
       , eitherPanic
       , inAssertMode
       , diffDoubleMap

       -- * Msgpack
       , msgpackFail
       , toMsgpackBinary
       , fromMsgpackBinary

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

       -- * TimeWarp helpers
       , WaitingDelta (..)
       , messageName'
       , logWarningLongAction
       , logWarningWaitOnce
       , logWarningWaitLinear
       , logWarningWaitInf
       , runWithRandomIntervals
       , waitRandomInterval

       -- * LRU
       , clearLRU

       -- * Instances
       -- ** SafeCopy (NonEmpty a)
       ) where

import           Control.Lens                  (Lens', LensLike', Magnified, Zoomed,
                                                lensRules, magnify, zoom)
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Control.Monad
import           Control.Monad.Fail            (fail)
import           Control.TimeWarp.Rpc          (Message (messageName), MessageName)
import           Control.TimeWarp.Timed        (Microsecond, MonadTimed (fork, wait),
                                                Second, for, killThread)
import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Binary (encode)
import qualified Data.Cache.LRU                as LRU
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Data.MessagePack              (MessagePack (..))
import qualified Data.MessagePack              as Msgpack
import           Data.SafeCopy                 (Contained, SafeCopy (..), base, contain,
                                                deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)
import           Data.String                   (String)
import           Data.Time.Units               (convertUnit)
import           Formatting                    (sformat, shown, stext, (%))
import           Language.Haskell.TH
import           Serokell.Util                 (VerificationRes)
import           Serokell.Util.Binary          as Binary (decodeFull)
import           System.Console.ANSI           (Color (..), ColorIntensity (Vivid),
                                                ConsoleLayer (Foreground),
                                                SGR (Reset, SetColor), setSGRCode)
import           System.Wlog                   (WithNamedLogger, logWarning)
import           Universum
import           Unsafe                        (unsafeInit, unsafeLast)

import           Pos.Crypto.Random             (randomNumber)
import           Pos.Util.Arbitrary            as UtilArbitrary
import           Pos.Util.NotImplemented       ()

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Eq, Ord, Show)

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Binary'
-- instance.
putCopyBinary :: Binary a => a -> Contained Cereal.Put
putCopyBinary x = contain $ safePut (Binary.encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Binary'
-- instance.
getCopyBinary :: Binary a => String -> Contained (Cereal.Get a)
getCopyBinary typeName = contain $ do
    bs <- safeGet
    case Binary.decodeFull bs of
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

----------------------------------------------------------------------------
-- MessagePack
----------------------------------------------------------------------------

-- | Report error in msgpack's fromObject.
msgpackFail :: Monad m => String -> m a
msgpackFail = Control.Monad.fail

-- TODO: pull request to data-messagepack
instance MessagePack a =>
         MessagePack (NonEmpty a) where
    toObject = toObject . toList
    fromObject = maybeToMsgpack errMsg . NE.nonEmpty <=< fromObject
      where
        errMsg = "Non-empty list is expected, but it's empty"
        maybeToMsgpack msg = maybe (msgpackFail msg) pure

-- | Convert instance of Binary into msgpack binary Object.
toMsgpackBinary :: Binary a => a -> Msgpack.Object
toMsgpackBinary = toObject . Binary.encode

-- | Extract ByteString from msgpack Object and decode it using Binary
-- instance.
fromMsgpackBinary
    :: (Binary a, Monad m)
    => String -> Msgpack.Object -> m a
fromMsgpackBinary typeName obj = do
    bs <- fromObject obj
    case Binary.decodeFull bs of
        Left err -> msgpackFail ("fromObject@" ++ typeName ++ ": " ++ err)
        Right x  -> return x

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

_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| unsafeInit xs ++ [y]) <$> f (unsafeLast xs)

-- TODO: we should try to get this one into safecopy itself though it's
-- unlikely that they will choose a different implementation (if they do
-- choose a different implementation we'll have to write a migration)
--
-- update: made a PR <https://github.com/acid-state/safecopy/pull/47>;
-- remove this instance when the pull request is merged
instance SafeCopy a => SafeCopy (NonEmpty a) where
    getCopy = contain $ do
        xs <- safeGet
        case NE.nonEmpty xs of
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

colorize :: Color -> Text -> Text
colorize color msg =
    mconcat
        [ toText (setSGRCode [SetColor Foreground Vivid color])
        , msg
        , toText (setSGRCode [Reset])
        ]

----------------------------------------------------------------------------
-- TimeWarp helpers
----------------------------------------------------------------------------

messageName' :: Message r => r -> MessageName
messageName' = messageName . (const Proxy :: a -> Proxy a)

-- | Data type to represent waiting strategy for printing warnings
-- if action take too much time.
data WaitingDelta
    = WaitOnce      Second              -- ^ wait s seconds and stop execution
    | WaitLinear    Second              -- ^ wait s, s * 2, s * 3  , s * 4  , ...      seconds
    | WaitGeometric Microsecond Double  -- ^ wait m, m * q, m * q^2, m * q^3, ... microseconds
    deriving (Show)

type CanLogInParallel m = (MonadIO m, MonadTimed m, WithNamedLogger m)

-- | Run action and print warning if it takes more time than expected.
logWarningLongAction :: CanLogInParallel m => WaitingDelta -> Text -> m a -> m a
logWarningLongAction delta actionTag action = do
    logThreadId <- fork $ waitAndWarn delta
    action      <* killThread logThreadId
  where
    printWarning t = logWarning $ sformat ("Action `"%stext%"` took more than "%shown)
                                  actionTag
                                  t

    -- TODO: avoid code duplication somehow
    waitAndWarn (WaitOnce      s  ) = wait (for s) >> printWarning s
    waitAndWarn (WaitLinear    s  ) = let waitLoop t = do
                                              wait $ for t
                                              printWarning t
                                              waitLoop (t + s)
                                      in waitLoop s
    waitAndWarn (WaitGeometric s q) = let waitLoop t = do
                                              wait $ for t
                                              printWarning (convertUnit t :: Second)
                                              waitLoop (round $ fromIntegral t * q)
                                      in waitLoop s

{- Helper functions to avoid dealing with data type -}

logWarningWaitOnce :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitOnce = logWarningLongAction . WaitOnce

logWarningWaitLinear :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitLinear = logWarningLongAction . WaitLinear

logWarningWaitInf :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitInf = logWarningLongAction . (`WaitGeometric` 1.3) . convertUnit

waitRandomInterval
    :: (MonadIO m, MonadTimed m)
    => Microsecond -> Microsecond -> m ()
waitRandomInterval minT maxT = do
    interval <-
        (+ minT) . fromIntegral <$>
        liftIO (randomNumber $ fromIntegral $ maxT - minT)
    wait $ for interval

runWithRandomIntervals
    :: (MonadIO m, MonadTimed m, WithNamedLogger m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervals minT maxT action = do
  waitRandomInterval minT maxT
  action
  runWithRandomIntervals minT maxT action

----------------------------------------------------------------------------
-- LRU cache
----------------------------------------------------------------------------

-- | Remove all items from LRU, retaining maxSize property.
clearLRU :: Ord k => LRU.LRU k v -> LRU.LRU k v
clearLRU = LRU.newLRU . LRU.maxSize

instance (Ord k, SafeCopy k, SafeCopy v) =>
         SafeCopy (LRU.LRU k v) where
    getCopy = contain $ LRU.fromList <$> safeGet <*> safeGet
    putCopy lru =
        contain $
        do safePut $ LRU.maxSize lru
           safePut $ LRU.toList lru
    errorTypeName _ = "LRU"
