{-# LANGUAGE TemplateHaskell #-}
module Pos.Binary.Core.Script () where

import           Universum

import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore
import qualified PlutusTypes.ConSig as PLTypes
import qualified PlutusTypes.Type   as PLTypes
import qualified Utils.ABT          as ABT
import qualified Utils.Names        as Names
import qualified Utils.Vars         as Vars

import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.SafeCopy      (SafeCopy(..))
import           Pos.Binary.Class   (Bi (..), deriveSimpleBi, Cons(..), Field(..),
                                     serialize', getCopyBi, putCopyBi, genericEncode, genericDecode)
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..), ScriptVersion)


instance Bi PLCore.Term => SafeCopy PLCore.Term where
    getCopy = getCopyBi
    putCopy = putCopyBi

instance Bi PLCore.Program => SafeCopy PLCore.Program where
    getCopy = getCopyBi
    putCopy = putCopyBi

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

instance Bi PLTypes.PolymorphicType where
  encode = genericEncode
  decode = genericDecode

instance Bi a => Bi (Names.Sourced a) where
  encode = genericEncode
  decode = genericDecode

instance Bi ABT.Variable where
  encode = genericEncode
  decode = genericDecode

instance Bi (f (ABT.Scope f)) => Bi (ABT.ABT f) where
  encode = genericEncode
  decode = genericDecode

instance Bi (f (ABT.Scope f)) => Bi (ABT.Scope f) where
  encode = genericEncode
  decode = genericDecode

instance Bi r => Bi (PLCore.ClauseF r) where
  encode = genericEncode
  decode = genericDecode

instance Bi a => Bi (PLCore.TermF a) where
  encode = genericEncode
  decode = genericDecode

instance Bi a => Bi (PLCore.PatternF a) where
  encode = genericEncode
  decode = genericDecode

instance Bi a => Bi (PLTypes.TypeF a) where
  encode = genericEncode
  decode = genericDecode

instance Bi PLCore.PrimData where
  encode = genericEncode
  decode = genericDecode

instance Bi PLCore.Program where
  encode = genericEncode
  decode = genericDecode

deriveSimpleBi ''Script [
    Cons 'Script [
        Field [| scrVersion :: ScriptVersion |],
        Field [| scrScript  :: LByteString   |]
    ]]

instance Hashable PLCore.Term where
    hashWithSalt s = hashWithSalt s . serialize'

instance Hashable PLCore.Program where
    hashWithSalt s = hashWithSalt s . serialize'
