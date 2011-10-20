{-# LANGUAGE EmptyDataDecls, TypeFamilies, UndecidableInstances #-}
module ReactiveVectors where

import Routes

data SVEmpty
data SVSignal a
data SVEvent a
data SVAppend v1 v2

type family EventRoute v
type instance EventRoute SVEmpty = EventRouteNowhere
type instance EventRoute (SVSignal a) = EventRouteNowhere
type instance EventRoute (SVEvent a) = EventRouteDone a
type instance EventRoute (SVAppend v1 v2) = CombineEventRoutes (EventRoute v1) (EventRoute v2)