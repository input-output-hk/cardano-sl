module Test.Pos.Binary.CommunicationSpec
    ( spec )
    where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, generate)
import           Test.QuickCheck.Monadic (assert)

import           Pos.Binary.Class (decodeFull, serialize')
import           Pos.Binary.Communication (serializeMsgSerializedBlock)
import           Pos.Block.Network.Types (MsgBlock (..), MsgSerializedBlock (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.DB.Class (Serialized (..))
import           Pos.Util.CompileInfo (withCompileInfo)

import           Test.Pos.Block.Logic.Mode (blockPropertyTestable)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..), bpGenBlock)
import           Test.Pos.Configuration (HasStaticConfigurations, withProvidedMagicConfig)

-- |
-- The binary encoding of `MsgSerializedBlock` using `serializeMsgSerializedBlock`
-- should be the same as the binary encoding of `MsgBlock`.
serializeMsgSerializedBlockSpec
    :: (HasStaticConfigurations) => ProtocolMagic -> Spec
serializeMsgSerializedBlockSpec pm = do
    prop desc $ blockPropertyTestable pm $ do
        (block, _) <- bpGenBlock pm (EnableTxPayload True) (InplaceDB True)
        let sb = Serialized $ serialize' block
        assert $ serializeMsgSerializedBlock (MsgSerializedBlock sb) == serialize' (MsgBlock block)
    prop descNoBlock $ blockPropertyTestable pm $ do
        let msg :: MsgSerializedBlock
            msg = MsgNoSerializedBlock "no block"
            msg' :: MsgBlock
            msg' = MsgNoBlock "no block"
        assert $ serializeMsgSerializedBlock msg == serialize' msg'
    where
    desc = "serializeMsgSerializedBlock for MsgSerializedBlock should create the same ByteString as serialize' for MsgBlock"
    descNoBlock = "serializeMsgSerializedBlock MsgNoSerializedBlock should create the same ByteString as serialize' for MsgNoBlock"


-- |
-- Deserialization of a serialized `MsgSerializedBlock` (with
-- `serializeMsgSerializedBlock`) should give back the original block.
deserializeSerilizedMsgSerializedBlockSpec
    :: (HasStaticConfigurations) => ProtocolMagic -> Spec
deserializeSerilizedMsgSerializedBlockSpec pm = do
    prop desc $ blockPropertyTestable pm $ do
        (block, _) <- bpGenBlock pm (EnableTxPayload True) (InplaceDB True)
        let sb = Serialized $ serialize' block
        let msg :: Either Text MsgBlock
            msg = decodeFull . BSL.fromStrict . serializeMsgSerializedBlock $ MsgSerializedBlock sb
        assert $ msg == Right (MsgBlock block)
    prop descNoBlock $ blockPropertyTestable pm $ do
        let msg :: MsgSerializedBlock
            msg = MsgNoSerializedBlock "no block"
        assert $ (decodeFull . BSL.fromStrict . serializeMsgSerializedBlock $ msg) == Right (MsgNoBlock "no block")
    where
    desc = "deserialization of a serialized MsgSerializedBlock message should give back corresponding MsgBlock"
    descNoBlock = "deserialization of a serialized MsgNoSerializedBlock message should give back corresponding MsgNoBlock"

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ withCompileInfo def $
    describe "Pos.Binary.Communication" $ do
        describe "serializeMsgSerializedBlock" (serializeMsgSerializedBlockSpec pm)
        describe "decode is left inverse of serializeMsgSerializedBlock" (deserializeSerilizedMsgSerializedBlockSpec pm)
