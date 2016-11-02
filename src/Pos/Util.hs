{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.Util
       (
       -- * Stuff for testing and benchmarking
         module UtilArbitrary

       -- * Various
       , Raw
       , readerToState

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
       , messageName'
       , logWarningLongAction

       -- * Instances
       -- ** SafeCopy (NonEmpty a)
       ) where

import           Control.Lens                  (Lens', LensLike', Magnified, Zoomed,
                                                lensRules, magnify, zoom)
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Control.Monad
import           Control.Monad.Fail            (fail)
import           Control.TimeWarp.Logging      (WithNamedLogger, logWarning)
import           Control.TimeWarp.Rpc          (Message (messageName), MessageName)
import           Control.TimeWarp.Timed        (MonadTimed (fork, wait), Second, for,
                                                killThread, mcs)

import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Binary (encode)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Data.MessagePack              (MessagePack (..))
import qualified Data.MessagePack              as Msgpack
import           Data.SafeCopy                 (Contained, SafeCopy (..), base, contain,
                                                deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)
import           Data.String                   (String)
import           Data.Time.Units               (toMicroseconds)
import           Formatting                    (int, sformat, stext, (%))
import           Language.Haskell.TH
import           Serokell.Util                 (VerificationRes)
import           System.Console.ANSI           (Color (..), ColorIntensity (Vivid),
                                                ConsoleLayer (Foreground),
                                                SGR (Reset, SetColor), setSGRCode)
import           Universum                     hiding (killThread)
import           Unsafe                        (unsafeInit, unsafeLast)

import           Serokell.Util.Binary          as Binary (decodeFull)

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

-- | Run action and print warning if it takes more time than expected.
logWarningLongAction
    :: (MonadIO m, MonadTimed m, WithNamedLogger m)
    => Text
    -> Second
    -> m a
    -> m ()
logWarningLongAction actionTag timeoutMs action = do
   logThreadId <- fork $ do
       wait $ for (fromIntegral $ toMicroseconds timeoutMs) mcs  -- so ugly
       logWarning $ sformat ("Action "%stext%" took more than "%int%" time")
                            actionTag
                            timeoutMs
   () <$ action  -- ignore result now; TODO: maybe return it later
   killThread logThreadId
