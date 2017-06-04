{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
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
       , SmartLimit
       , reifyMsgLimit
       , recvLimited

       , MaxSize (..)
       , (<+>)
       , withLimitedLength'
       ) where

import           Universum

import qualified Data.Binary                as Bin
import           Data.Reflection            (Reifies (..), reify)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.Protocol (ConversationActions (..), Message)
import qualified Pos.DB.Class               as DB

-- | Specifies limit for given type @t@.
newtype Limit t = Limit Byte32
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data SizeOrLimit t = SizeL Byte | LimitL (Limit t)
    deriving (Eq, Show)

-- TODO: instances for Num

infixl 4 <+>
(<+>) :: Limit (a -> b) -> Limit a -> Limit b
Limit x <+> Limit y = Limit $ x + y

instance Functor Limit where
    fmap _ (Limit x) = Limit x

-- | Specifies type of limit on incoming message size.
-- Useful when the type has several limits and choice depends on constructor.
class Limiter l where
    sizeGet :: l -> Bin.Get Byte32
    addLimit :: Byte -> l -> l

instance Limiter (SizeOrLimit t) where
    sizeGet (SizeL l)  = pure l
    sizeGet (LimitL l) = limitGet l

    addLimit a (SizeL l)  = SizeL $ a + l
    addLimit a (LimitL l) = addLimit a l

instance Limiter (Limit t) where
    sizeGet (Limit l) = do
        sz <- fromIntegral <$> Bin.get
        if sz > l
            then fail $ "Limit exceed: " ++ show sz
                          ++ " is more than " ++ show l
            else return sz
    addLimit a = (Limit a +)

-- | Specifies limit on message length.
-- Deserialization would fail if incoming data size exceeded this limit.
-- At serialisation stage message size is __not__ checked.
class Limiter (LimitType a) =>
      MessageLimited a where
    type LimitType a :: *
    type LimitType a = Limit a

    getMsgLenLimit :: DB.MonadGStateCore m => Proxy a -> m (LimitType a)

    default getMsgLenLimit :: ( LimitType a ~ Limit a
                              , MessageLimitedPure a
                              , DB.MonadGStateCore m
                              ) => Proxy a -> m (LimitType a)
    getMsgLenLimit _ = pure msgLenLimit

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
    msgLenLimit :: SizeOrLimit a

instance MessageLimitedPure a => MessageLimitedPure (Maybe a) where
    msgLenLimit = Just <$> msgLenLimit + 1

instance ( MessageLimitedPure a
         , MessageLimitedPure b
         )
         => MessageLimitedPure (Either a b) where
    msgLenLimit = 1 + max (Left <$> msgLenLimit) (Right <$> msgLenLimit)

instance ( MessageLimitedPure a
         , MessageLimitedPure b
         )
         => MessageLimitedPure (a, b) where
    msgLenLimit = (,) <$> msgLenLimit <+> msgLenLimit

instance ( MessageLimitedPure a
         , MessageLimitedPure b
         , MessageLimitedPure c
         )
         => MessageLimitedPure (a, b, c) where
    msgLenLimit = (,,) <$> msgLenLimit <+> msgLenLimit <+> msgLenLimit

instance MessageLimitedPure Bool where
    msgLenLimit = 1

-- instance MessageLimitedPure Word32 where
    -- msgLenLimit = 4

-- | Sets size limit to deserialization instances via @s@ parameter
-- (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
-- @l@ parameter specifies type of limit and is generally determined by @a@
newtype LimitedLengthExt s l a = LimitedLength
    { withLimitedLength :: a
    } deriving (Eq, Ord, Show)

withLimitedLength' :: Proxy s -> LimitedLengthExt s l a -> a
withLimitedLength' _ = withLimitedLength

deriving instance Message a => Message (LimitedLengthExt s l a)

type LimitedLength s a = LimitedLengthExt s (Limit a) a

type SmartLimit s a = LimitedLengthExt s (LimitType a) a

-- | Used to provide type @s@, which carries limit on length
-- of message @a@ (via Data.Reflection).
reifyMsgLimit
    :: forall a m b. (DB.MonadGStateCore m, MessageLimited a)
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
-- where an alternative exists, maximum available size is chosen.
-- This is required at first place to generate lists of max available size.
newtype MaxSize a = MaxSize
    { getOfMaxSize :: a
    } deriving (Eq, Ord, Show, Bi, Functor, MessageLimitedPure)
