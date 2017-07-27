{-# LANGUAGE TypeFamilies #-}

-- | Parameters used by blockchain generator.

module Pos.Generator.Block.Param
       ( InvSecretsMap
       , unInvSecretsMap
       , mkInvSecretsMap
       , AllSecrets (..)
       , HasAllSecrets (..)
       , TxGenParams (..)
       , HasTxGenParams (..)
       , BlockGenParams (..)
       , HasBlockGenParams (..)
       ) where

import           Universum

import           Control.Lens.TH     (makeClassy)
import           Data.Default        (Default (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, formatToString, int, (%))
import qualified Prelude
import           Serokell.Util       (listJson, pairF)

import           Pos.Core            (BlockCount, StakeholderId, addressHash)
import           Pos.Crypto          (SecretKey, toPublic)

-- | This map stores effectively provides inverse of 'hash' and
-- 'toPublic' functions. It's quite useful in tests and block
-- generator. (/Inv/ means /inverse/).
newtype InvSecretsMap = InvSecretsMap
    { unInvSecretsMap :: HashMap StakeholderId SecretKey
    } deriving (Container, Monoid, NontrivialContainer)

type instance Element InvSecretsMap = SecretKey

-- | Make 'InvSecretsMap' from a list of secret keys.
mkInvSecretsMap :: [SecretKey] -> InvSecretsMap
mkInvSecretsMap =
    let toSecretPair sk = (addressHash (toPublic sk), sk)
    in InvSecretsMap . HM.fromList . map toSecretPair

-- | All secrets in the system.
--
-- TODO: probably VSS keys should be added here at some point.
data AllSecrets = AllSecrets
    { _asSecretKeys :: !InvSecretsMap
    -- ^ Secret keys of all stakeholders participating in the system.
    }

makeClassy ''AllSecrets

instance Buildable AllSecrets where
    build AllSecrets {..} =
        bprint ("AllSecrets {\n"%
                "  secret keys: "%int%" items\n"%
                "  stakeholders: "%listJson%"\n"%
                "}\n")
            (length _asSecretKeys)
            (HM.keys $ unInvSecretsMap _asSecretKeys)

-- | Parameters for transactions payload generation.
data TxGenParams = TxGenParams
    { _tgpTxCountRange :: !(Word, Word)
    -- ^ Such (a, d), that there block will include some x ∈ [a, a+d)
    -- transactions. Set to (y,0) to disable tx generation.
    , _tgpMaxOutputs   :: !Word
    -- ^ Maximum number of tx outputs.
    }

makeClassy ''TxGenParams

instance Buildable TxGenParams where
    build TxGenParams {..} = do
        let (a,d) = _tgpTxCountRange
        bprint ("TxGenParams {\n"%
                "  tx count [from,to): "%pairF%"\n"%
                "  max outputs: "%int%"\n"%
                "}\n")
            (a,a+d)
            _tgpMaxOutputs

instance Show TxGenParams where
    show = formatToString build

instance Default TxGenParams where
    def = TxGenParams { _tgpTxCountRange = (0,5)
                      , _tgpMaxOutputs = 4
                      }

-- | Parameters for blockchain generation. Probably they come from the outside.
data BlockGenParams = BlockGenParams
    { _bgpSecrets     :: !AllSecrets
    -- ^ Secret keys of all stakeholders from genesis 'Utxo'.  They
    -- are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.  In
    -- testing environment we often want to have inverse of 'hash' and
    -- 'toPublic'.
    , _bgpBlockCount  :: !BlockCount
    -- ^ Number of blocks to generate.
    , _bgpTxGenParams :: !TxGenParams
    -- ^ Transaction generation parameters.
    , _bgpInplaceDB   :: !Bool
    -- ^ Whether to extend existing DB.
    }

makeClassy ''BlockGenParams

instance Buildable BlockGenParams where
    build BlockGenParams {..} =
        bprint ("BlockGenParams {\n"%
                "  secrets: "%build%"\n"%
                "  number of blocks: "%int%"\n"%
                "  tx gen params: "%build%"\n"%
                "}\n")
            _bgpSecrets
            _bgpBlockCount
            _bgpTxGenParams

instance HasAllSecrets BlockGenParams where
    allSecrets = bgpSecrets

instance HasTxGenParams BlockGenParams where
    txGenParams = bgpTxGenParams

instance Show BlockGenParams where
    show = formatToString build
