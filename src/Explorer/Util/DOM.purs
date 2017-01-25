module Explorer.Util.DOM
    ( scrollTop
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)

foreign import scrollTopImpl :: forall eff. Eff (dom :: DOM | eff) Unit

scrollTop :: forall eff. Eff (dom :: DOM | eff) Unit
scrollTop = scrollTopImpl
