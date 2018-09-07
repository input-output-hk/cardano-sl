module Test.Pos.Chain.Delegation.Example
       ( exampleLightDlgIndices
       , exampleProxySKBlockInfo
       , staticHeavyDlgIndexes
       , staticProxySKHeavys
       ) where

import           Universum

import           Data.List (zipWith4, (!!))

import           Pos.Chain.Delegation (HeavyDlgIndex (..), LightDlgIndices (..),
                     ProxySKBlockInfo, ProxySKHeavy)
import           Pos.Core (EpochIndex (..))
import           Pos.Crypto (ProtocolMagic (..), safeCreatePsk)

import           Test.Pos.Core.ExampleHelpers (examplePublicKey,
                     examplePublicKeys, staticSafeSigners)

staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5,1,3,27,99,247]

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0..5]

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4 safeCreatePsk
                               staticProtocolMagics staticSafeSigners
                               (examplePublicKeys 1 6) staticHeavyDlgIndexes

exampleProxySKBlockInfo :: ProxySKBlockInfo
exampleProxySKBlockInfo = Just (staticProxySKHeavys !! 0, examplePublicKey)

exampleLightDlgIndices :: LightDlgIndices
exampleLightDlgIndices = LightDlgIndices (EpochIndex 7, EpochIndex 88)
