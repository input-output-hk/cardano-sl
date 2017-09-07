-- | Binary instances for Genesis data (such as 'BalanceDistribution')

module Pos.Binary.Core.Genesis () where

import           Universum

import           Pos.Binary.Class        (Bi (..), decodeListLen, encodeListLen,
                                          enforceSize, matchSize)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Core.Types   ()
import           Pos.Core.Address        ()
import           Pos.Core.Genesis.Types  (BalanceDistribution (..), GenesisCoreData (..),
                                          GenesisDelegation, GenesisWStakeholders (..),
                                          mkGenesisCoreData, mkGenesisDelegation,
                                          unGenesisDelegation)
import           Pos.Util.Util           (eitherToFail)

instance Bi BalanceDistribution where
    encode input = case input of
      FlatBalances w c             ->
          encodeListLen 3 <> encode (0 :: Word8) <> encode w <> encode c
      RichPoorBalances w1 c1 w2 c2 ->
          encodeListLen 5 <> encode (1 :: Word8)
                          <> encode w1
                          <> encode c1
                          <> encode w2
                          <> encode c2
      ExponentialBalances w mc     ->
          encodeListLen 3 <> encode (2 :: Word8) <> encode w <> encode mc
      CustomBalances c             ->
          encodeListLen 2 <> encode (3 :: Word8) <> encode c
    decode = do
      len <- decodeListLen
      tag <- decode @Word8
      case tag of
          0 -> do
              matchSize 3 "BalanceDistribution.FlatBalances" len
              FlatBalances        <$> decode <*> decode
          1 -> do
              matchSize 5 "BalanceDistribution.RichPoorBalances" len
              RichPoorBalances    <$> decode <*> decode <*> decode <*> decode
          2 -> do
              matchSize 3 "BalanceDistribution.ExponentialBalances" len
              ExponentialBalances <$> decode <*> decode
          3 -> do
              matchSize 2 "BalanceDistribution.CustomBalances" len
              CustomBalances      <$> decode
          _ -> fail "Pos.Binary.Genesis: BalanceDistribution: invalid tag"

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
