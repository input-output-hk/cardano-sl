{-# LANGUAGE TemplateHaskell #-}

module Pos.Ssc.GodTossing.SecretStorage.Types
       (
         GtSecret
       , GtSecretStorage (..)
       ) where

import           Data.Default                  (Default, def)
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import           Universum

import           Pos.Crypto                    (PublicKey)
import           Pos.Ssc.GodTossing.Types.Base (Opening, SignedCommitment)
import           Pos.Types                     (SlotId, unflattenSlotId)

type GtSecret = (PublicKey, SignedCommitment, Opening)

data GtSecretStorage = GtSecretStorage
    {
      -- | Secret that we are using for the current epoch.
      _dsCurrentSecret     :: !(Maybe GtSecret)
    , -- | Last slot we are aware of.
      _dsLastProcessedSlot :: !SlotId
    }

deriveSafeCopySimple 0 'base ''GtSecretStorage

instance Default GtSecretStorage where
    def =
        GtSecretStorage
        {
          _dsCurrentSecret = Nothing
        , _dsLastProcessedSlot = unflattenSlotId 0
        }
