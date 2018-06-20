{-# LANGUAGE CPP #-}

-- | Helper instances for acid-state to remove boilerplate and introduce
-- redundant instances for other data types.

module Serokell.AcidState.Instances where

import           Control.Exception   (throw)
import           Control.Monad.Catch (MonadThrow (throwM))

import qualified Data.Time.Units     as Time
import           Data.Acid           (Query, Update)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS hiding (HashSet)
import           Data.SafeCopy       (SafeCopy (..), contain, safeGet,
                                      safePut)

instance MonadThrow (Update s) where
    throwM = throw

instance (Eq a, Hashable a, SafeCopy a) => SafeCopy (HashSet a) where
    putCopy = contain . safePut . HS.toList
    getCopy = contain $ HS.fromList <$> safeGet

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
    putCopy = contain . safePut . HM.toList
    getCopy = contain $ HM.fromList <$> safeGet

#define SAFECOPY_TIME(T, TS)                     \
  instance SafeCopy T where {                    \
    getCopy = contain (fromInteger <$> safeGet); \
    putCopy = contain . safePut . toInteger;     \
    errorTypeName _ = TS }                       \

SAFECOPY_TIME(Time.Fortnight, "Fortnight")
SAFECOPY_TIME(Time.Week, "Week")
SAFECOPY_TIME(Time.Day, "Day")
SAFECOPY_TIME(Time.Hour, "Hour")
SAFECOPY_TIME(Time.Minute, "Minute")
SAFECOPY_TIME(Time.Second, "Second")
SAFECOPY_TIME(Time.Millisecond, "Millisecond")
SAFECOPY_TIME(Time.Microsecond, "Microsecond")
SAFECOPY_TIME(Time.Nanosecond, "Nanosecond")
SAFECOPY_TIME(Time.Picosecond, "Picosecond")
SAFECOPY_TIME(Time.Femtosecond, "Femtosecond")
SAFECOPY_TIME(Time.Attosecond, "Attosecond")

#undef SAFECOPY_TIME
