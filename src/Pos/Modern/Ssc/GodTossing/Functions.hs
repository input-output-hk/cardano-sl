{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Modern.Ssc.GodTossing.Functions
       (
         hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate
       ) where

import qualified Data.HashMap.Strict                     as HM
import           Universum

import           Pos.Crypto                              (PublicKey)
import           Pos.Modern.Ssc.GodTossing.Storage.Types (GtGlobalState (..))
import           Pos.Types.Address                       (AddressHash)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

hasCommitment :: AddressHash PublicKey -> GtGlobalState -> Bool
hasCommitment addr = HM.member addr . _gsCommitments

hasOpening :: AddressHash PublicKey -> GtGlobalState -> Bool
hasOpening addr = HM.member addr . _gsOpenings

hasShares :: AddressHash PublicKey -> GtGlobalState -> Bool
hasShares addr = HM.member addr . _gsShares

hasVssCertificate :: AddressHash PublicKey -> GtGlobalState -> Bool
hasVssCertificate addr = HM.member addr . _gsVssCertificates
