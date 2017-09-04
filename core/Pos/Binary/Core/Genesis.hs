-- | Binary instances for Genesis data (such as `StakeDistribution`)

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), decodeListLen, encodeListLen,
                                          enforceSize, matchSize)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), GenesisDelegation,
                                          GenesisWStakeholders (..),
                                          StakeDistribution (..), mkGenesisCoreData,
                                          mkGenesisDelegation, unGenesisDelegation)
import           Pos.Util.Util           (eitherToFail)

instance Bi StakeDistribution where
    encode input = case input of
      FlatStakes w c             -> encodeListLen 3 <> encode (0 :: Word8) <> encode w <> encode c
      RichPoorStakes w1 c1 w2 c2 -> encodeListLen 5 <> encode (1 :: Word8)
                                                    <> encode w1
                                                    <> encode c1
                                                    <> encode w2
                                                    <> encode c2
      ExponentialStakes w mc     -> encodeListLen 3 <> encode (2 :: Word8) <> encode w <> encode mc
      CustomStakes c             -> encodeListLen 2 <> encode (3 :: Word8) <> encode c
    decode = do
      len <- decodeListLen
      tag <- decode @Word8
      case tag of
          0 -> do
              matchSize 3 "StakeDistribution.FlatStakes" len
              FlatStakes        <$> decode <*> decode
          1 -> do
              matchSize 5 "StakeDistribution.RichPoorStakes" len
              RichPoorStakes    <$> decode <*> decode <*> decode <*> decode
          2 -> do
              matchSize 3 "StakeDistribution.ExponentialStakes" len
              ExponentialStakes <$> decode <*> decode
          3 -> do
              matchSize 2 "StakeDistribution.CustomStakes" len
              CustomStakes      <$> decode
          _ -> fail "Pos.Binary.Genesis: StakeDistribution: invalid tag"

instance Bi GenesisWStakeholders where
    encode (GenesisWStakeholders m) = encode m
    decode = GenesisWStakeholders <$> decode

instance Bi GenesisDelegation where
    encode (unGenesisDelegation -> m) = encode (toList m)
    decode = eitherToFail . mkGenesisDelegation =<< decode

instance Bi GenesisCoreData where
    encode (UnsafeGenesisCoreData addr stakes delega) =
        encode (addr, stakes, delega)
    decode = do
        enforceSize "GenesisCoreData" 3
        addrDistribution <- decode
        bootstrapStakeholders <- decode
        delega <- decode
        case mkGenesisCoreData addrDistribution bootstrapStakeholders delega of
            Left e  -> fail $ "Couldn't construct genesis data: " <> e
            Right x -> pure x
