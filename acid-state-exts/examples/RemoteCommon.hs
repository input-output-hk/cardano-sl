{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module RemoteCommon where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data StressState = StressState !Int deriving (Typeable)

$(deriveSafeCopy 0 'base ''StressState)

------------------------------------------------------
-- The transaction we will execute over the state.

pokeState :: Update StressState ()
pokeState = do StressState i <- get
               put (StressState (i+1))

queryState :: Query StressState Int
queryState = do StressState i <- ask
                return i

clearState :: Update StressState ()
clearState = put $ StressState 0

$(makeAcidic ''StressState ['pokeState, 'queryState, 'clearState])
