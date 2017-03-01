{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Limits.Types
       ( Limit (..)
       , Limiter (..)
       , MessageLimited (..)
       , MessageLimitedPure (..)

       , LimitedLengthExt (..)
       , LimitedLength
       , reifyMsgLimit
       , recvLimited

       , MaxSize (..)
       , (<+>)
       ) where

import           Data.Binary                (Get)
import           Data.Reflection            (Reifies (..), reify)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Binary.Class           (Bi (..))
import           Pos.Communication.Protocol (ConversationActions (..))
import qualified Pos.DB.Limits              as DB

-- | Specifies limit for given type @t@.
newtype Limit t = Limit Byte
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

infixl 4 <+>
(<+>) :: Limit (a -> b) -> Limit a -> Limit b
Limit x <+> Limit y = Limit $ x + y

instance Functor Limit where
    fmap _ (Limit x) = Limit x

-- | Specifies type of limit on incoming message size.
-- Useful when the type has several limits and choice depends on constructor.
class Limiter l where
    limitGet :: l -> Get a -> Get a
    addLimit :: Byte -> l -> l

-- | Specifies limit on message length.
-- Deserialization would fail if incoming data size exceeded this limit.
-- At serialisation stage message size is __not__ checked.
class Limiter (LimitType a) => MessageLimited a where
    type LimitType a :: *
    getMsgLenLimit :: DB.MonadDBLimits m => Proxy a -> m (LimitType a)

-- | Pure analogy to `MessageLimited`. Allows to easily get message length
-- limit for simple types.
--
-- All instances are encouraged to be covered with tests
-- (using `Test.Pos.Util.msgLenLimitedTest`).
--
-- If you're going to add instance and have no idea regarding limit value,
-- and your type's size is essentially bounded (doesn't contain list-like
-- structures and doesn't depend on global parameters), you can do as follows:
-- 1) Create instance with limit @1@.
-- 2) Add test case, run - it would fail and report actual size.
-- 3) Insert that value into instance.
class MessageLimitedPure a where
    msgLenLimit :: Limit a


-- | Sets size limit to deserialization instances via @s@ parameter
-- (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
-- @l@ parameter specifies type of limit and is generally determined by @a@
newtype LimitedLengthExt s l a = LimitedLength
    { withLimitedLength :: a
    } deriving (Eq, Ord, Show)

type LimitedLength s a = LimitedLengthExt s (Limit a) a

-- | Used to provide type @s@, which carries limit on length
-- of message @a@ (via Data.Reflection).
reifyMsgLimit
    :: forall a m b. (DB.MonadDBLimits m, MessageLimited a)
    => Proxy a
    -> (forall s. Reifies s (LimitType a) => Proxy s -> m b)
    -> m b
reifyMsgLimit _ f = do
    lengthLimit <- getMsgLenLimit $ Proxy @a
    reify lengthLimit f

recvLimited
    :: forall s rcv snd m.
       Monad m
    => ConversationActions snd (LimitedLength s rcv) m -> m (Maybe rcv)
recvLimited conv = fmap withLimitedLength <$> recv conv

-- | Wrapper for `Arbitrary` instances to indicate that
-- where an alternative exists, maximal available size is choosen.
-- This is required at first place to generate lists of max available size.
newtype MaxSize a = MaxSize
    { getOfMaxSize :: a
    } deriving (Eq, Ord, Show, Bi, Functor, MessageLimitedPure)
