module Explorer.I18n.Lenses where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Explorer.I18n.Types


title :: forall a b r. Lens.Lens { "title" :: a | r } { "title" :: b | r } a b
title = Lens.lens _."title" (_ { "title" = _ })

subtitle :: forall a b r. Lens.Lens { "subtitle" :: a | r } { "subtitle" :: b | r } a b
subtitle = Lens.lens _."subtitle" (_ { "subtitle" = _ })

back :: forall a b r. Lens.Lens { "back" :: a | r } { "back" :: b | r } a b
back = Lens.lens _."back" (_ { "back" = _ })

transaction :: forall a b r. Lens.Lens { "transaction" :: a | r } { "transaction" :: b | r } a b
transaction = Lens.lens _."transaction" (_ { "transaction" = _ })

transactions :: forall a b r. Lens.Lens { "transactions" :: a | r } { "transactions" :: b | r } a b
transactions = Lens.lens _."transactions" (_ { "transactions" = _ })

transactionFeed :: forall a b r. Lens.Lens { "transactionFeed" :: a | r } { "transactionFeed" :: b | r } a b
transactionFeed = Lens.lens _."transactionFeed" (_ { "transactionFeed" = _ })

address :: forall a b r. Lens.Lens { "address" :: a | r } { "address" :: b | r } a b
address = Lens.lens _."address" (_ { "address" = _ })

calculator :: forall a b r. Lens.Lens { "calculator" :: a | r } { "calculator" :: b | r } a b
calculator = Lens.lens _."calculator" (_ { "calculator" = _ })

version :: forall a b r. Lens.Lens { "version" :: a | r } { "version" :: b | r } a b
version = Lens.lens _."version" (_ { "version" = _ })

summary :: forall a b r. Lens.Lens { "summary" :: a | r } { "summary" :: b | r } a b
summary = Lens.lens _."summary" (_ { "summary" = _ })

block :: forall a b r. Lens.Lens { "block" :: a | r } { "block" :: b | r } a b
block = Lens.lens _."block" (_ { "block" = _ })

hashes :: forall a b r. Lens.Lens { "hashes" :: a | r } { "hashes" :: b | r } a b
hashes = Lens.lens _."hashes" (_ { "hashes" = _ })

nav :: forall a b r. Lens.Lens { "nav" :: a | r } { "nav" :: b | r } a b
nav = Lens.lens _."nav" (_ { "nav" = _ })

height :: forall a b r. Lens.Lens { "height" :: a | r } { "height" :: b | r } a b
height = Lens.lens _."height" (_ { "height" = _ })

age :: forall a b r. Lens.Lens { "age" :: a | r } { "age" :: b | r } a b
age = Lens.lens _."age" (_ { "age" = _ })

totalSent :: forall a b r. Lens.Lens { "totalSent" :: a | r } { "totalSent" :: b | r } a b
totalSent = Lens.lens _."totalSent" (_ { "totalSent" = _ })

relayedBy :: forall a b r. Lens.Lens { "relayedBy" :: a | r } { "relayedBy" :: b | r } a b
relayedBy = Lens.lens _."relayedBy" (_ { "relayedBy" = _ })

sizeKB :: forall a b r. Lens.Lens { "sizeKB" :: a | r } { "sizeKB" :: b | r } a b
sizeKB = Lens.lens _."sizeKB" (_ { "sizeKB" = _ })

home :: forall a b r. Lens.Lens { "home" :: a | r } { "home" :: b | r } a b
home = Lens.lens _."home" (_ { "home" = _ })

blockchain :: forall a b r. Lens.Lens { "blockchain" :: a | r } { "blockchain" :: b | r } a b
blockchain = Lens.lens _."blockchain" (_ { "blockchain" = _ })

market :: forall a b r. Lens.Lens { "market" :: a | r } { "market" :: b | r } a b
market = Lens.lens _."market" (_ { "market" = _ })

charts :: forall a b r. Lens.Lens { "charts" :: a | r } { "charts" :: b | r } a b
charts = Lens.lens _."charts" (_ { "charts" = _ })

tools :: forall a b r. Lens.Lens { "tools" :: a | r } { "tools" :: b | r } a b
tools = Lens.lens _."tools" (_ { "tools" = _ })
