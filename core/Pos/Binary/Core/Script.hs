{-# LANGUAGE TemplateHaskell #-}
module Pos.Binary.Core.Script () where

import           Universum

import qualified Data.Store         as Store
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Data.SafeCopy      (SafeCopy(..))
import           Pos.Binary.Class   (Bi (..), Encoding, Decoder, deriveSimpleBi, Cons(..), Field(..),
                                    getCopyBi, putCopyBi)
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..), ScriptVersion)


instance Bi PLCore.Term => SafeCopy PLCore.Term where
    getCopy = getCopyBi
    putCopy = putCopyBi

instance Bi PLCore.Program => SafeCopy PLCore.Program where
    getCopy = getCopyBi
    putCopy = putCopyBi


-- Derive `Bi` instances by piggybacking
-- on Store instances, in preparation for a proper
-- rewrite down the line.
storeEncode :: Store.Store a => a -> Encoding
storeEncode = encode @ByteString . Store.encode

storeDecode :: Store.Store a => Decoder s a
storeDecode = do
    storeBs <- decode @ByteString
    either (fail . show) return (Store.decode storeBs)

instance Bi PLCore.Term where
  encode = storeEncode
  decode = storeDecode

instance Bi PLCore.Program where
  encode = storeEncode
  decode = storeDecode

deriveSimpleBi ''Script [
    Cons 'Script [
        Field [| scrVersion :: ScriptVersion |],
        Field [| scrScript  :: LByteString   |]
    ]]
