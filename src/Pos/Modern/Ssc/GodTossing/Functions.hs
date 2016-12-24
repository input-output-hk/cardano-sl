{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Modern.Ssc.GodTossing.Functions
       (
         hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate
       ) where

import qualified Data.HashMap.Strict              as HM
import           Universum

import           Pos.Ssc.GodTossing.Storage.Types (GtGlobalState (..))
import           Pos.Types.Types                  (Address (..))

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

hasCommitment :: Address -> GtGlobalState -> Bool
hasCommitment addr = HM.member addr . _gsCommitments

hasOpening :: Address -> GtGlobalState -> Bool
hasOpening addr = HM.member addr . _gsOpenings

hasShares :: Address -> GtGlobalState -> Bool
hasShares addr = HM.member addr . _gsShares

hasVssCertificate :: Address -> GtGlobalState -> Bool
hasVssCertificate addr = HM.member addr . _gsVssCertificates
