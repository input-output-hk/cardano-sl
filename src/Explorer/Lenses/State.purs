module Explorer.Lenses.State where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Explorer.Types.State


lang :: forall a b r. Lens.Lens { "lang" :: a | r } { "lang" :: b | r } a b
lang = Lens.lens _."lang" (_ { "lang" = _ })

route :: forall a b r. Lens.Lens { "route" :: a | r } { "route" :: b | r } a b
route = Lens.lens _."route" (_ { "route" = _ })

viewStates :: forall a b r. Lens.Lens { "viewStates" :: a | r } { "viewStates" :: b | r } a b
viewStates = Lens.lens _."viewStates" (_ { "viewStates" = _ })

dashboard :: forall a b r. Lens.Lens { "dashboard" :: a | r } { "dashboard" :: b | r } a b
dashboard = Lens.lens _."dashboard" (_ { "dashboard" = _ })

blocksExpanded :: forall a b r. Lens.Lens { "blocksExpanded" :: a | r } { "blocksExpanded" :: b | r } a b
blocksExpanded = Lens.lens _."blocksExpanded" (_ { "blocksExpanded" = _ })

transactionsExpanded :: forall a b r. Lens.Lens { "transactionsExpanded" :: a | r } { "transactionsExpanded" :: b | r } a b
transactionsExpanded = Lens.lens _."transactionsExpanded" (_ { "transactionsExpanded" = _ })

selectedApiCode :: forall a b r. Lens.Lens { "selectedApiCode" :: a | r } { "selectedApiCode" :: b | r } a b
selectedApiCode = Lens.lens _."selectedApiCode" (_ { "selectedApiCode" = _ })

_Curl :: Lens.Prism' DashboardAPICode Prelude.Unit
_Curl = Lens.prism (Prelude.const Curl) unwrap
  where
    unwrap Curl = Either.Right Prelude.unit
    unwrap y = Either.Left y

_Node :: Lens.Prism' DashboardAPICode Prelude.Unit
_Node = Lens.prism (Prelude.const Node) unwrap
  where
    unwrap Node = Either.Right Prelude.unit
    unwrap y = Either.Left y

_JQuery :: Lens.Prism' DashboardAPICode Prelude.Unit
_JQuery = Lens.prism (Prelude.const JQuery) unwrap
  where
    unwrap JQuery = Either.Right Prelude.unit
    unwrap y = Either.Left y
