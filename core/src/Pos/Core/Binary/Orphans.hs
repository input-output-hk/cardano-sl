{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Binary.Orphans () where

import           Universum

import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize as Cereal
import           Data.Time.Units (Microsecond, Millisecond)
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term as PLCore
import qualified PlutusTypes.ConSig as PLTypes
import qualified PlutusTypes.Type as PLTypes
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import qualified Utils.ABT as ABT
import qualified Utils.Names as Names
import qualified Utils.Vars as Vars

import           Pos.Binary.Class (Bi (..), genericDecode, genericEncode,
                     serialize')
import           Pos.Binary.SafeCopy (getCopyBi, putCopyBi)
import           Pos.Core.Script ()
import qualified Pos.Util.Modifier as MM

instance Bi Vars.FreeVar where
    encode = genericEncode
    decode = genericDecode

instance Bi Vars.MetaVar where
    encode = genericEncode
    decode = genericDecode

instance Bi Vars.BoundVar where
    encode = genericEncode
    decode = genericDecode

instance Bi PLTypes.TyConSig where
    encode = genericEncode
    decode = genericDecode

instance Bi PLTypes.ConSig where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (Names.Sourced a) where
    encode = genericEncode
    decode = genericDecode

instance Bi ABT.Variable where
    encode = genericEncode
    decode = genericDecode

instance (Typeable f, Bi (f (ABT.Scope f))) => Bi (ABT.ABT f) where
    encode = genericEncode
    decode = genericDecode

instance (Typeable f, Bi (f (ABT.Scope f))) => Bi (ABT.Scope f) where
    encode = genericEncode
    decode = genericDecode

instance (Typeable r, Bi r) => Bi (PLCore.ClauseF r) where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (PLCore.TermF a) where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (PLTypes.TypeF a) where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.SimplePattern where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.PrimData where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.Program where
    encode = genericEncode
    decode = genericDecode

instance Hashable PLCore.Term where
    hashWithSalt s = hashWithSalt s . serialize'

instance Hashable PLCore.Program where
    hashWithSalt s = hashWithSalt s . serialize'

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


instance Bi PLCore.Term => SafeCopy PLCore.Term where
    getCopy = getCopyBi
    putCopy = putCopyBi

instance Bi PLCore.Program => SafeCopy PLCore.Program where
    getCopy = getCopyBi
    putCopy = putCopyBi


deriveSafeCopySimple 0 'base ''Microsecond
deriveSafeCopySimple 0 'base ''Millisecond
