{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Pos.Util.Util
       (
       -- * Existential types
         Some(..)
       , Some1(..)
       , applySome
       , liftLensSome
       , liftGetterSome

       , maybeThrow
       , getKeys

       -- * Ether
       , ether

       -- * Instances
       -- ** Lift Byte
       -- ** FromJSON Byte
       -- ** ToJSON Byte
       -- ** MonadFail (Either s), assuming IsString s
       -- ** NFData Millisecond
       -- ** NFData Microsecond
       -- ** Buildable Attosecond
       -- ** Buildable Femtosecond
       -- ** Buildable Picosecond
       -- ** Buildable Nanosecond
       -- ** Buildable Millisecond
       -- ** Buildable Microsecond
       -- ** Buildable Second
       -- ** Buildable Minute
       -- ** Buildable Hour
       -- ** Buildable Day
       -- ** Buildable Week
       -- ** Buildable Fortnight

       -- ** Ether instances
       -- *** CanLog Ether.StateT
       -- *** HasLoggerName Ether.StateT
       ) where

import           Control.Lens               (ALens', Getter, Getting, cloneLens, to)
import qualified Control.Monad.Ether              as Ether
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Control.Monad.Trans.Ether.Tagged as Ether
import           Control.Monad.Trans.Lift.Local   (LiftLocal(..))
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.HashSet               (fromMap)
import           Data.Text.Buildable        (build)
import           Data.Time.Units            (Attosecond, Day, Femtosecond, Fortnight,
                                             Hour, Microsecond, Millisecond, Minute,
                                             Nanosecond, Picosecond, Second, Week,
                                             toMicroseconds)
import qualified Language.Haskell.TH.Syntax       as TH
import qualified Prelude
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import           System.Wlog                      (CanLog, HasLoggerName (..))
import           Universum

----------------------------------------------------------------------------
-- Some
----------------------------------------------------------------------------

-- | Turn any constraint into an existential type! Example:
--
-- @
-- foo :: Some Show -> String
-- foo (Some s) = show s
-- @
data Some c where
    Some :: c a => a -> Some c

instance Show (Some Show) where
    show (Some s) = show s

-- | Like 'Some', but for @* -> *@ types – for instance, @Some1 Functor ()@
data Some1 c a where
    Some1 :: c f => f a -> Some1 c a

instance Functor (Some1 Functor) where
    fmap f (Some1 x) = Some1 (fmap f x)

-- | Apply a function requiring the constraint
applySome :: (forall a. c a => a -> r) -> (Some c -> r)
applySome f (Some x) = f x

-- | Turn a lens into something operating on 'Some'. Useful for many types
-- like 'HasDifficulty', 'IsHeader', etc.
liftLensSome :: (forall a. c a => ALens' a b)
             -> Lens' (Some c) b
liftLensSome l =
    \f (Some a) -> Some <$> cloneLens l f a

-- | Like 'liftLensSome', but for getters.
liftGetterSome :: (forall a. c a => Getting b a b) -> Getter (Some c) b
liftGetterSome l = \f (Some a) -> Some <$> to (view l) f a

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance TH.Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes

instance IsString s => MonadFail (Either s) where
    fail = Left . fromString

instance NFData Millisecond where
    rnf ms = deepseq (toInteger ms) ()

instance NFData Microsecond where
    rnf ms = deepseq (toInteger ms) ()

----------------------------------------------------------------------------
-- Orphan Buildable instances for time-units
----------------------------------------------------------------------------

instance Buildable Attosecond  where build = build @String . show
instance Buildable Femtosecond where build = build @String . show
instance Buildable Picosecond  where build = build @String . show
instance Buildable Nanosecond  where build = build @String . show
instance Buildable Millisecond where build = build @String . show
instance Buildable Second      where build = build @String . show
instance Buildable Minute      where build = build @String . show
instance Buildable Hour        where build = build @String . show
instance Buildable Day         where build = build @String . show
instance Buildable Week        where build = build @String . show
instance Buildable Fortnight   where build = build @String . show

-- | Special case. We don't want to print greek letter mu in console because
-- it breaks things sometimes.
instance Buildable Microsecond where
    build = build . (++ "mcs") . show . toMicroseconds

----------------------------------------------------------------------------
-- Ether instances
----------------------------------------------------------------------------

instance
  (Monad m, MonadTrans t, Monad (t m), CanLog m) =>
  CanLog (Ether.TaggedTrans tag t m)

instance
  (LiftLocal t, Monad m, HasLoggerName m) =>
  HasLoggerName (Ether.TaggedTrans tag t m) where
    getLoggerName = lift getLoggerName
    modifyLoggerName = liftLocal getLoggerName modifyLoggerName

----------------------------------------------------------------------------
-- Not instances
----------------------------------------------------------------------------

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

-- | Create HashSet from HashMap's keys
getKeys :: HashMap k v -> HashSet k
getKeys = fromMap . void

-- | Make a Reader or State computation work in an Ether transformer. Useful
-- to make lenses work with Ether.
ether :: trans m a -> Ether.TaggedTrans tag trans m a
ether = Ether.pack
