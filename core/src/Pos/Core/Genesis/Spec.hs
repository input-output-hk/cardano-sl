module Pos.Core.Genesis.Spec
       ( GenesisSpec (..)
       , mkGenesisSpec
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import qualified Data.HashMap.Strict as HM
import           Serokell.Util (allDistinct)

import           Pos.Core.Common (SharedSeed)
import           Pos.Core.Update (BlockVersionData)

import           Pos.Core.Genesis.AvvmBalances
import           Pos.Core.Genesis.Delegation
import           Pos.Core.Genesis.Initializer
import           Pos.Core.Genesis.ProtocolConstants

-- | Specification how to generate full genesis data.
data GenesisSpec = UnsafeGenesisSpec
    { gsAvvmDistr         :: !GenesisAvvmBalances
    -- ^ Genesis data describes avvm utxo.
    , gsFtsSeed           :: !SharedSeed
    -- ^ Seed for FTS for 0-th epoch.
    , gsHeavyDelegation   :: !GenesisDelegation
    -- ^ Genesis state of heavyweight delegation. Will be concatenated
    -- with genesis delegation for bootstrap stakeholders if
    -- 'tiUseHeavyDlg' is 'True'.
    , gsBlockVersionData  :: !BlockVersionData
    -- ^ Genesis 'BlockVersionData'.
    , gsProtocolConstants :: !GenesisProtocolConstants
    -- ^ Other constants which affect consensus.
    , gsInitializer       :: !GenesisInitializer
    -- ^ Other data which depend on genesis type.
    } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''GenesisSpec

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec
    :: GenesisAvvmBalances
    -> SharedSeed
    -> GenesisDelegation
    -> BlockVersionData
    -> GenesisProtocolConstants
    -> GenesisInitializer
    -> Either String GenesisSpec
mkGenesisSpec avvmDistr seed delega bvd pc specType = do
    let avvmKeys = HM.keys $ getGenesisAvvmBalances avvmDistr
    unless (allDistinct avvmKeys) $
        throwError $ "mkGenesisSpec: there are duplicates in avvm balances"

    -- All checks passed
    pure $ UnsafeGenesisSpec avvmDistr seed delega bvd pc specType
