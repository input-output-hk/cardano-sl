{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module OrphanedLenses where

import           Control.Lens (makeLensesWith)

import           Pos.Chain.Update (BlockVersionData, BlockVersionModifier)
import           Pos.Util (postfixLFields)

makeLensesWith postfixLFields ''BlockVersionModifier
makeLensesWith postfixLFields ''BlockVersionData
