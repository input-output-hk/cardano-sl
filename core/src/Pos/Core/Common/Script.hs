module Pos.Core.Common.Script
       ( Script (..)
       , ScriptVersion
       , Script_v0
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, int, (%))
import qualified PlutusCore.Program as PLCore

import qualified Pos.Binary.Class as Bi

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script
    { scrVersion :: ScriptVersion -- ^ Version
    , scrScript  :: ByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance NFData Script
instance Hashable Script

instance Buildable Script where
    build Script{..} = bprint ("<script v"%int%">") scrVersion

Bi.deriveSimpleBi ''Script [
    Bi.Cons 'Script [
        Bi.Field [| scrVersion :: ScriptVersion |],
        Bi.Field [| scrScript  :: ByteString   |]
    ]]

deriveSafeCopySimple 0 'base ''Script

-- | Deserialized script (i.e. an AST), version 0.
type Script_v0 = PLCore.Program
