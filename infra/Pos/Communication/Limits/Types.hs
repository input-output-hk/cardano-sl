{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Limits.Types
       ( Limit (..)
       , (<+>)

       , MessageLimited (..)
       , MessageLimitedPure (..)


       , recvLimited

       -- Commented out because every use of it everywhere else seems to also
       -- be commented out.
       -- FIXME decide whether to keep it or trash it.
       --, MaxSize (..)
       ) where

import           Universum


import           Pos.Communication.Protocol (ConversationActions (..))
import qualified Pos.DB.Class               as DB

-- | A limit on the length of something (in bytes).
--   TODO should check for overflow in the Num instance.
--   Although, if the limit is anywhere near maxBound :: Word32 then something
--   is almost certainly amiss.
newtype Limit t = Limit { getLimit :: Word32 }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Functor Limit where
    fmap _ (Limit x) = Limit x

infixl 4 <+>
(<+>) :: Limit (a -> b) -> Limit a -> Limit b
Limit x <+> Limit y = Limit $ x + y

-- | Defines how to determine the limit of some type's serialized representation
--   using some particular state. See 'recvLimited'.
class MessageLimited a where
    getMsgLenLimit :: DB.MonadGState m => Proxy a -> m (Limit a)
    default getMsgLenLimit :: ( MessageLimitedPure a
                              , DB.MonadGState m
                              ) => Proxy a -> m (Limit a)
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
--
-- TODO FIXME can we get rid of this class?
-- Its instances are basically tied up with the Binary instances; they assume
-- to know how they are defined.
class MessageLimitedPure a where
    msgLenLimit :: Limit a

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

-- | Use the MessageLimited instance to determine the length limit of a
--   'rcv' type within the monadic context, and then receive at most that
--   many bytes. If more than that many bytes come in before a parse then
--   an exception is raised.
recvLimited
    :: forall rcv snd m .
       ( Monad m, DB.MonadGState m, MessageLimited rcv )
    => ConversationActions snd rcv m -> m (Maybe rcv)
recvLimited conv = getMsgLenLimit (Proxy @rcv) >>= recv conv . getLimit

-- | Wrapper for `Arbitrary` instances to indicate that
-- where an alternative exists, maximum available size is chosen.
-- This is required at first place to generate lists of max available size.
-- newtype MaxSize a = MaxSize
--     { getOfMaxSize :: a
--     } deriving (Eq, Ord, Show, Bi.Bi, Functor, MessageLimitedPure)
