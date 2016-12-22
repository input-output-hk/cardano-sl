{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Pos.Ssc.Class.Helpers
       (
         SscHelpersClass (..)
       ) where

import           Data.Tagged          (Tagged)
import           Serokell.Util.Verify (VerificationRes)
-- import           Universum

import           Pos.Ssc.Class.Types  (Ssc (..))
import           Pos.Types.Types      (MainBlockHeader)

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)
