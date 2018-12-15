{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Binary.Orphans () where

import           Universum

import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize as Cereal
import           Data.Time.Units (Microsecond, Millisecond)
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)

import qualified Pos.Util.Modifier as MM

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
    putCopy = contain . safePut . HM.toList
    getCopy = contain $ HM.fromList <$> safeGet

instance Cereal.Serialize Byte where
    get = fromBytes <$> Cereal.get
    put = Cereal.put . toBytes

instance SafeCopy Byte

instance (SafeCopy k, SafeCopy v, Eq k, Hashable k) => SafeCopy (MM.MapModifier k v) where
    getCopy = contain $ MM.fromHashMap <$> safeGet
    putCopy mm = contain $ safePut (MM.toHashMap mm)


deriveSafeCopySimple 0 'base ''Microsecond
deriveSafeCopySimple 0 'base ''Millisecond
