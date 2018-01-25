{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Orphan instances for external types/classes.

module Pos.Util.Orphans
       (
       -- * Instances
       -- ** Lift Byte
       -- ** Lift HashMap
       -- ** FromJSON Byte, ToJSON Byte
       -- ** Hashable Byte
       -- ** HasLoggerName (MonadPseudoRandom drg)

       -- ** Hashable
       -- *** Millisecond, Microsecond

       -- ** NFData
       -- *** Millisecond, Microsecond

       -- ** MonadRandom
       -- *** monad transformers
       -- *** Gen (from QuickCheck)

       -- ** Buildable
       -- *** "Data.Time.Units" types
       -- *** @()@
       ) where

import           Universum

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import           Control.Monad.Trans.Resource (MonadResource (..), ResourceT, transResourceT)
import qualified Crypto.Random as Rand
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Hashable (Hashable (hashWithSalt))
import qualified Data.HashMap.Strict as HM
import           Data.Tagged (Tagged (Tagged))
import           Data.Text.Buildable (build)
import           Data.Time.Units (Attosecond, Day, Femtosecond, Fortnight, Hour, Microsecond,
                                  Millisecond, Minute, Nanosecond, Picosecond, Second, Week,
                                  toMicroseconds)
import           Data.Typeable (typeRep)
import qualified Ether
import qualified Formatting as F
import qualified Language.Haskell.TH.Syntax as TH
import           Mockable (ChannelT, Counter, Distribution, Gauge, MFunctor' (..), Mockable (..),
                           Promise, SharedAtomicT, SharedExclusiveT, ThreadId)
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import           System.Wlog (CanLog, HasLoggerName (..), LoggerNameBox (..))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Monadic (PropertyM (..))

----------------------------------------------------------------------------
-- Orphan miscellaneous instances
----------------------------------------------------------------------------

instance MonadReader r m => MonadReader r (PropertyM m) where
    ask = lift ask
    local f (MkPropertyM propertyM) =
        MkPropertyM $ \hole -> local f <$> propertyM hole

instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v) where
    lift x = let l = HM.toList x in [|HM.fromList l|]

instance Hashable Byte where
    hashWithSalt i = hashWithSalt i . toInteger

instance TH.Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes

instance Rand.DRG drg => HasLoggerName (Rand.MonadPseudoRandom drg) where
    askLoggerName = pure "MonadPseudoRandom"
    modifyLoggerName = flip const

instance {-# OVERLAPPABLE #-}
         (MonadTrans t, Functor (t m), Monad (t m), Rand.MonadRandom m)
         => Rand.MonadRandom (t m) where
    getRandomBytes = lift . Rand.getRandomBytes

-- TODO: use the 'vec' package for traversable N-products
data Five a = Five a a a a a
    deriving (Functor, Foldable, Traversable)

five :: a -> Five a
five a = Five a a a a a

instance Rand.MonadRandom QC.Gen where
    getRandomBytes n = do
        Five a b c d e <- sequenceA . five $ QC.choose (minBound, maxBound)
        pure $ fst $ Rand.randomBytesGenerate n (Rand.drgNewTest (a,b,c,d,e))

----------------------------------------------------------------------------
-- Hashable
----------------------------------------------------------------------------

instance Hashable Millisecond where
    hashWithSalt i a = hashWithSalt i (toInteger a)

instance Hashable Microsecond where
    hashWithSalt i a = hashWithSalt i (toInteger a)

----------------------------------------------------------------------------
-- NFData
----------------------------------------------------------------------------

instance NFData Millisecond where
    rnf ms = rnf (toInteger ms)

instance NFData Microsecond where
    rnf ms = rnf (toInteger ms)

----------------------------------------------------------------------------
-- Orphan Buildable instances
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

-- | This orphan instance is sometimes useful and why not have it?
instance Buildable () where
    build _ = "()"

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
    build tt@(Tagged v) = F.bprint ("Tagged " F.% F.shown F.% " " F.% F.build) ts v
      where
        ts = typeRep proxy
        proxy = (const Proxy :: Tagged s a -> Proxy s) tt

----------------------------------------------------------------------------
-- MonadResource/ResourceT
----------------------------------------------------------------------------

instance LiftLocal ResourceT where
    liftLocal _ l f = hoist (l f)

instance {-# OVERLAPPABLE #-}
    (MonadResource m, MonadTrans t, Applicative (t m),
     MonadBase IO (t m), MonadIO (t m), MonadThrow (t m)) =>
        MonadResource (t m)
  where
    liftResourceT = lift . liftResourceT

instance CanLog m => CanLog (ResourceT m)
instance (Monad m, HasLoggerName m) => HasLoggerName (ResourceT m) where
    askLoggerName = lift askLoggerName
    modifyLoggerName = transResourceT . modifyLoggerName

----------------------------------------------------------------------------
-- Instances required by 'ether'
----------------------------------------------------------------------------

instance
    (Monad m, MonadTrans t, Monad (t m), CanLog m) =>
        CanLog (Ether.TaggedTrans tag t m)

instance (Monad m, CanLog m) => CanLog (IdentityT m)

instance
    (LiftLocal t, Monad m, HasLoggerName m) =>
        HasLoggerName (Ether.TaggedTrans tag t m)
  where
    askLoggerName = lift askLoggerName
    modifyLoggerName = liftLocal askLoggerName modifyLoggerName

instance
    (Monad m, HasLoggerName m) => HasLoggerName (IdentityT m)
  where
    askLoggerName = lift askLoggerName
    modifyLoggerName = liftLocal askLoggerName modifyLoggerName

deriving instance LiftLocal LoggerNameBox

instance {-# OVERLAPPABLE #-}
    (Monad m, MFunctor t) => MFunctor' t m n
  where
    hoist' = hoist

instance
    (Mockable d m, MFunctor' d (IdentityT m) m) =>
        Mockable d (IdentityT m)
  where
    liftMockable dmt = IdentityT $ liftMockable $ hoist' runIdentityT dmt

unTaggedTrans :: Ether.TaggedTrans tag t m a -> t m a
unTaggedTrans (Ether.TaggedTrans tma) = tma

instance
      (Mockable d (t m), Monad (t m),
       MFunctor' d (Ether.TaggedTrans tag t m) (t m)) =>
          Mockable d (Ether.TaggedTrans tag t m)
  where
    liftMockable dmt =
      Ether.TaggedTrans $ liftMockable $ hoist' unTaggedTrans dmt

type instance ThreadId (IdentityT m) = ThreadId m
type instance Promise (IdentityT m) = Promise m
type instance SharedAtomicT (IdentityT m) = SharedAtomicT m
type instance Counter (IdentityT m) = Counter m
type instance Distribution (IdentityT m) = Distribution m
type instance SharedExclusiveT (IdentityT m) = SharedExclusiveT m
type instance Gauge (IdentityT m) = Gauge m
type instance ChannelT (IdentityT m) = ChannelT m

type instance ThreadId (Ether.TaggedTrans tag t m) = ThreadId m
type instance Promise (Ether.TaggedTrans tag t m) = Promise m
type instance SharedAtomicT (Ether.TaggedTrans tag t m) = SharedAtomicT m
type instance Counter (Ether.TaggedTrans tag t m) = Counter m
type instance Distribution (Ether.TaggedTrans tag t m) = Distribution m
type instance SharedExclusiveT (Ether.TaggedTrans tag t m) = SharedExclusiveT m
type instance Gauge (Ether.TaggedTrans tag t m) = Gauge m
type instance ChannelT (Ether.TaggedTrans tag t m) = ChannelT m
