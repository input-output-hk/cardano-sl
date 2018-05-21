{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..), unliftIO, withUnliftIO)
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
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import           System.Wlog (CanLog, HasLoggerName (..), LoggerNameBox (..))

----------------------------------------------------------------------------
-- Orphan miscellaneous instances
----------------------------------------------------------------------------

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

instance {-# OVERLAPPABLE #-}
    (MonadResource m, MonadTrans t, MonadIO (t m)) =>
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

instance MonadUnliftIO (t m) => MonadUnliftIO (Ether.TaggedTrans tag t m) where
    {-# INLINE askUnliftIO #-}
    askUnliftIO =
        Ether.TaggedTrans $
        withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . \case Ether.TaggedTrans trans -> trans))
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        Ether.TaggedTrans $
        withRunInIO $ \run ->
        inner (run . \case Ether.TaggedTrans trans -> trans)
