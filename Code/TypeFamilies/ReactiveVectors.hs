{-# LANGUAGE EmptyDataDecls, TypeFamilies, GADTs, UndecidableInstances #-}
module ReactiveVectors where

-- Routes
data RouteNowhere
data RouteDone a                           
data RouteFirst r                           
data RouteSecond r                           
data RouteBoth r1 r2
                            
type family CombineRouteType r1 r2
type instance CombineRouteType RouteNowhere RouteNowhere = RouteNowhere
type instance CombineRouteType RouteNowhere (RouteDone a) = RouteSecond (RouteDone a)
type instance CombineRouteType RouteNowhere (RouteFirst r) = RouteSecond (RouteFirst r)
type instance CombineRouteType RouteNowhere (RouteSecond r) = RouteSecond (RouteSecond r)
type instance CombineRouteType RouteNowhere (RouteBoth r1 r2) = RouteSecond (RouteBoth r1 r2)
type instance CombineRouteType (RouteDone a) RouteNowhere = RouteFirst (RouteDone a)
type instance CombineRouteType (RouteFirst r) RouteNowhere = RouteFirst (RouteFirst r)
type instance CombineRouteType (RouteSecond r) RouteNowhere = RouteFirst (RouteSecond r)
type instance CombineRouteType (RouteBoth r1 r2) RouteNowhere = RouteFirst (RouteBoth r1 r2)
type instance CombineRouteType (RouteDone a) (RouteDone b) = RouteBoth (RouteDone a) (RouteDone b)
type instance CombineRouteType (RouteDone a) (RouteFirst r) = RouteBoth (RouteDone a) (RouteFirst r)
type instance CombineRouteType (RouteDone a) (RouteSecond r) = RouteBoth (RouteDone a) (RouteSecond r)
type instance CombineRouteType (RouteDone a) (RouteBoth r1 r2) = RouteBoth (RouteDone a) (RouteBoth r1 r2)
type instance CombineRouteType (RouteFirst r) (RouteDone b) = RouteBoth (RouteFirst r) (RouteDone b)
type instance CombineRouteType (RouteFirst r1) (RouteFirst r2) = RouteBoth (RouteFirst r1) (RouteFirst r2)
type instance CombineRouteType (RouteFirst r1) (RouteSecond r2) = RouteBoth (RouteFirst r1) (RouteSecond r2)
type instance CombineRouteType (RouteFirst r1) (RouteBoth r2 r3) = RouteBoth (RouteFirst r1) (RouteBoth r2 r3)
type instance CombineRouteType (RouteSecond r) (RouteDone b) = RouteBoth (RouteSecond r) (RouteDone b)
type instance CombineRouteType (RouteSecond r1) (RouteFirst r2) = RouteBoth (RouteSecond r1) (RouteFirst r2)
type instance CombineRouteType (RouteSecond r1) (RouteSecond r2) = RouteBoth (RouteSecond r1) (RouteSecond r2)
type instance CombineRouteType (RouteSecond r1) (RouteBoth r2 r3) = RouteBoth (RouteSecond r1) (RouteBoth r2 r3)
type instance CombineRouteType (RouteBoth r1 r2) (RouteDone b) = RouteBoth (RouteBoth r1 r2) (RouteDone b)
type instance CombineRouteType (RouteBoth r1 r2) (RouteFirst r3) = RouteBoth (RouteBoth r1 r2) (RouteFirst r3)
type instance CombineRouteType (RouteBoth r1 r2) (RouteSecond r3) = RouteBoth (RouteBoth r1 r2) (RouteSecond r3)
type instance CombineRouteType (RouteBoth r1 r2) (RouteBoth r3 r4) = RouteBoth (RouteBoth r1 r2) (RouteBoth r3 r4)
     
-- Reactive Vectors
data RVEmpty
data RVSignal a
data RVEvent a
data RVAppend v1 v2


-- Event routes
data EventRoute v r where
  EventRouteEmpty          :: EventRoute RVEmpty RouteNowhere
  EventRouteSignal         :: EventRoute (RVSignal a) RouteNowhere
  EventRouteEvent          :: a -> EventRoute (RVEvent a) (RouteDone a)
  EventRouteNoEvent        :: EventRoute (RVEvent a) (RouteDone a)
  EventRouteCombine        :: EventRoute v1 (EventRouteType v1) 
                              -> EventRoute v2 (EventRouteType v2) 
                              -> EventRoute (RVAppend v1 v2) (CombineRouteType (EventRouteType v1) (EventRouteType v2))

type family EventRouteType v
type instance EventRouteType RVEmpty          = RouteNowhere
type instance EventRouteType (RVSignal a)     = RouteNowhere
type instance EventRouteType (RVEvent a)      = RouteDone a
type instance EventRouteType (RVAppend v1 v2) = CombineRouteType (EventRouteType v1) (EventRouteType v2)
  
                                                
-- Signal routes
data SignalRoute v r where
  SignalRouteEmpty   :: SignalRoute RVEmpty RouteNowhere
  SignalRouteEvent   :: a -> SignalRoute (RVEvent a) (RouteDone a)
  SignalRouteNoEvent :: SignalRoute (RVEvent a) (RouteDone a)
  SignalRouteSignal  :: a -> SignalRoute (RVSignal a) (RouteDone a)
  SignalRouteCombine :: SignalRoute v1 (SignalRouteType v1)
                        -> SignalRoute v2 (SignalRouteType v2)
                        -> SignalRoute (RVAppend v1 v2) (CombineRouteType (SignalRouteType v1) (SignalRouteType v2))

type family SignalRouteType v
type instance SignalRouteType RVEmpty          = RouteNowhere
type instance SignalRouteType (RVEvent a)      = RouteDone a
type instance SignalRouteType (RVSignal a)     = RouteDone a
type instance SignalRouteType (RVAppend v1 v2) = CombineRouteType (SignalRouteType v1) (SignalRouteType v2)

-- Event route combinators
combineEventRoute :: EventRoute v1 (EventRouteType v1) 
                     -> EventRoute v2 (EventRouteType v2) 
                     -> EventRoute (RVAppend v1 v2) (CombineRouteType (EventRouteType v1) (EventRouteType v2))
combineEventRoute r1 r2 = EventRouteCombine r1 r2

eventRouteFirst :: EventRoute (RVAppend v1 v2) (CombineRouteType (EventRouteType v1) (EventRouteType v2)) -> EventRoute v1 (EventRouteType v1)
eventRouteFirst (EventRouteCombine r1 _) = r1

eventRouteSecond :: EventRoute (RVAppend v1 v2) (CombineRouteType (EventRouteType v1) (EventRouteType v2)) -> EventRoute v2 (EventRouteType v2)
eventRouteSecond (EventRouteCombine _ r2) = r2

-- Signal route combinators
combineSignalRoute :: SignalRoute v1 (SignalRouteType v1)
                      -> SignalRoute v2 (SignalRouteType v2)
                      -> SignalRoute (RVAppend v1 v2) (CombineRouteType (SignalRouteType v1) (SignalRouteType v2))
combineSignalRoute r1 r2 = SignalRouteCombine r1 r2

signalRouteFirst :: SignalRoute (RVAppend v1 v2) (CombineRouteType (SignalRouteType v1) (SignalRouteType v2)) -> SignalRoute v1 (SignalRouteType v1)
signalRouteFirst (SignalRouteCombine r1 _) = r1

signalRouteSecond :: SignalRoute (RVAppend v1 v2) (CombineRouteType (SignalRouteType v1) (SignalRouteType v2)) -> SignalRoute v2 (SignalRouteType v2)
signalRouteSecond (SignalRouteCombine _ r2) = r2

-- Convert a signal route to an event route
signalRouteToEventRoute :: SignalRoute v (SignalRouteType v) -> EventRoute v (EventRouteType v)
signalRouteToEventRoute SignalRouteEmpty = EventRouteEmpty
signalRouteToEventRoute (SignalRouteEvent x) = EventRouteEvent x
signalRouteToEventRoute SignalRouteNoEvent = EventRouteNoEvent
signalRouteToEventRoute (SignalRouteSignal _) = EventRouteSignal
signalRouteToEventRoute (SignalRouteCombine r1 r2) = EventRouteCombine (signalRouteToEventRoute r1) (signalRouteToEventRoute r2)

