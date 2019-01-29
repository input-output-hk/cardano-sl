{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.WalletLayer.Kernel.Settings (
    getNodeSettings
  ) where

import           Universum

import qualified Data.Text as T
import           Data.Time.Units (Millisecond)

import           Pos.Core (TxFeePolicy (..))
import qualified Pos.Node.API as Node
import           Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as KNode

import           Paths_cardano_wallet (version)

getNodeSettings :: MonadIO m => Kernel.PassiveWallet -> m V1.NodeSettings
getNodeSettings w = liftIO $
    V1.NodeSettings
        <$> (V1 <$> KNode.getTipSlotId node)
        <*> (mkSlotDuration <$> KNode.getNextEpochSlotDuration node)
        <*> (V1 <$> KNode.getSlotCount node)
        <*> (V1 <$> KNode.curSoftwareVersion node)
        <*> pure (V1 version)
        <*> (mkGitRevision <$> KNode.compileInfo node)
        <*> (Node.mkMaxTxSize . fromIntegral <$> KNode.getMaxTxSize node)
        <*> (Node.fromCorePolicy <$> (KNode.getFeePolicy node >>= mkFeePolicy))
        <*> (mkSecurityParameter <$> KNode.getSecurityParameter node)
  where
    mkSlotDuration :: Millisecond -> V1.SlotDuration
    mkSlotDuration = V1.mkSlotDuration . fromIntegral

    mkGitRevision :: CompileTimeInfo -> Text
    mkGitRevision = T.replace "\n" mempty . ctiGitRevision

    mkSecurityParameter (KNode.SecurityParameter i) =
        Node.SecurityParameter i

    mkFeePolicy = \case
        TxFeePolicyTxSizeLinear a -> return a
        _ -> fail "getNodeSettings: Unsupported / Unknown fee policy."

    node :: NodeStateAdaptor IO
    node = w ^. Kernel.walletNode
