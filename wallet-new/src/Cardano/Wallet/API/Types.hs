{-- Types shared between different API versions. --}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.API.Types where

import Data.Aeson
import Test.QuickCheck

data APIVersion = V0
                | V1
                deriving (Eq, Enum, Bounded)

instance Arbitrary APIVersion where
  arbitrary = elements [minBound .. maxBound]

instance Show APIVersion where
  show V0 = "v0"
  show V1 = "v1"

instance ToJSON APIVersion where
  toJSON x = object ["version" .= show x]
