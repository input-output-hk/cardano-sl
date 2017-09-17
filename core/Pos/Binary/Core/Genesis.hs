-- | Binary instances for Genesis data

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (GenesisDelegation, GenesisWStakeholders (..),
                                          mkGenesisDelegation, unGenesisDelegation)
import           Pos.Util.Util           (eitherToFail)

instance Bi GenesisWStakeholders where
    encode (GenesisWStakeholders m) = encode m
    decode = GenesisWStakeholders <$> decode

instance Bi GenesisDelegation where
    encode (unGenesisDelegation -> m) = encode (toList m)
    decode = eitherToFail . mkGenesisDelegation =<< decode
