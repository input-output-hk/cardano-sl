{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits.Types
       ( Limit (..)
       , (<+>)

       , MessageLimited (..)

       , recvLimited

       -- Commented out because every use of it everywhere else seems to also
       -- be commented out.
       -- FIXME decide whether to keep it or trash it.
       --, MaxSize (..)
       ) where

import           Universum

import           Pos.Communication.Protocol (ConversationActions (..))

-- | A limit on the length of something (in bytes).
--   TODO should check for overflow in the Num instance.
--   Although, if the limit is anywhere near maxBound :: Word32 then something
--   is almost certainly amiss.
newtype Limit t = Limit { getLimit :: Word32 }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Functor Limit where
    fmap _ (Limit x) = Limit x

-- TODO: use <*> instead of <+>
infixl 4 <+>
(<+>) :: Limit (a -> b) -> Limit a -> Limit b
Limit x <+> Limit y = Limit $ x + y

-- | Defines how to determine the limit of some type's serialized
-- representation using some particular state. See 'recvLimited'.
--
-- TODO FIXME can we get rid of this class?
-- Its instances are basically tied up with the Binary instances; they assume
-- to know how they are defined.
class MessageLimited a m where
    getMsgLenLimit :: Proxy a -> m (Limit a)

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

instance (Applicative m, MessageLimited a m) => MessageLimited (Maybe a) m where
    getMsgLenLimit _ = (\lim -> Just <$> lim + 1) <$> (getMsgLenLimit Proxy)

instance ( MessageLimited a m
         , MessageLimited b m
         , Applicative m
         )
         => MessageLimited (Either a b) m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
      f limA limB = 1 + max (Left <$> limA) (Right <$> limB)

instance ( MessageLimited a m
         , MessageLimited b m
         , Applicative m
         )
         => MessageLimited (a, b) m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
      f limA limB = (,) <$> limA <+> limB

instance ( MessageLimited a m
         , MessageLimited b m
         , MessageLimited c m
         , Applicative m
         )
         => MessageLimited (a, b, c) m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
      f limA limB limC = (,,) <$> limA <+> limB <+> limC

instance Applicative m => MessageLimited Bool m where
    getMsgLenLimit _ = pure 1

-- | Use the MessageLimited instance to determine the length limit of a
--   'rcv' type within the monadic context, and then receive at most that
--   many bytes. If more than that many bytes come in before a parse then
--   an exception is raised.
recvLimited
    :: forall rcv snd m .
       ( Monad m, MessageLimited rcv m )
    => ConversationActions snd rcv m -> m (Maybe rcv)
recvLimited conv = getMsgLenLimit (Proxy @rcv) >>= recv conv . getLimit

-- | Wrapper for `Arbitrary` instances to indicate that
-- where an alternative exists, maximum available size is chosen.
-- This is required at first place to generate lists of max available size.
-- newtype MaxSize a = MaxSize
--     { getOfMaxSize :: a
--     } deriving (Eq, Ord, Show, Bi.Bi, Functor, MessageLimitedPure)
