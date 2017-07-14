{-# LANGUAGE TemplateHaskell #-}
module Pos.Binary.Core.Script () where

import           Universum

import qualified Data.Store         as Store
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..), label, labelP, labelS,
                                     putField)
import qualified Pos.Binary.Cbor     as Cbor
import qualified Codec.CBOR.Encoding as Cbor.Encoding
import qualified Codec.CBOR.Decoding as Cbor.Decoding
import           Pos.Core.Script    ()
import           Pos.Core.Types     (Script (..), ScriptVersion)



-- Derive `Cbor.Bi` instances by piggybacking
-- on Store instances, in preparation for a proper
-- rewrite down the line.
storeEncode :: Store.Store a => a -> Cbor.Encoding.Encoding
storeEncode = Cbor.encode @ByteString . Store.encode

storeDecode :: Store.Store a => Cbor.Decoding.Decoder s a
storeDecode = do
    storeBs <- Cbor.decode @ByteString
    either (fail . show) return (Store.decode storeBs)

instance Bi PLCore.Term where
    size = Store.size
    {-# INLINE size #-}
    get = label "PLCore.Term" $ Store.peek
    {-# INLINE get #-}
    put = labelP "PLCore.Term" . Store.poke
    {-# INLINE put #-}

instance Cbor.Bi PLCore.Term where
  encode = storeEncode
  decode = storeDecode

instance Bi PLCore.Program where
    size = Store.size
    {-# INLINE size #-}
    get = label "PLCore.Program" $ Store.peek
    {-# INLINE get #-}
    put = labelP "PLCore.Program" . Store.poke
    {-# INLINE put #-}

instance Cbor.Bi PLCore.Program where
  encode = storeEncode
  decode = storeDecode

instance Bi Script where
    get = label "Script" $ do
        UnsignedVarInt scrVersion <- get
        scrScript                 <- get
        pure Script{..}
    sizeNPut = labelS "Script" $
        putField (UnsignedVarInt . scrVersion) <>
        putField scrScript

Cbor.deriveSimpleBi ''Script [
    Cbor.Cons 'Script [
        Cbor.Field [| scrVersion :: ScriptVersion |],
        Cbor.Field [| scrScript  :: ByteString   |]
    ]]
