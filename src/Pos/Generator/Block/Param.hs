-- | Parameters used by blockchain generator.

module Pos.Generator.Block.Param
       ( AllSecrets (..)
       , HasAllSecrets (..)
       , BlockGenParams (..)
       , HasBlockGenParams (..)
       ) where

import           Universum

import           Control.Lens.TH (makeClassy)

import           Pos.Core        (BlockCount, StakeholderId)
import           Pos.Crypto      (SecretKey)

-- | All secret keys in the system. In testing environment we often
-- want to have inverse of 'hash' and 'toPublic'.
--
-- TODO: probably VSS keys should be added here at some point.
data AllSecrets = AllSecrets
    { _asSecretKeys :: !(HashMap StakeholderId SecretKey)
    -- ^ Secret keys of all stakeholders from the genesis 'Utxo'.
    }

makeClassy ''AllSecrets

-- | Parameters for blockchain generation. Probably they come from the outside.
data BlockGenParams = BlockGenParams
    { _bgpSecrets    :: !AllSecrets
    -- ^ Secret keys of all stakeholders from genesis 'Utxo'.  They
    -- are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.  In
    -- testing environment we often want to have inverse of 'hash' and
    -- 'toPublic'.
    , _bgpBlockCount :: !BlockCount
    -- ^ Number of blocks to generate.
    }

makeClassy ''BlockGenParams

instance HasAllSecrets BlockGenParams where
    allSecrets = bgpSecrets
