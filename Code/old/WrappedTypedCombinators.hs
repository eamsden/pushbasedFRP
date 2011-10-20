{-# LANGUAGE MultiParamTypeClasses, RankNQuantifiers, FlexibleInstances, FunctionalDependencies, GADTs #-}
module WrappedTypedCombinators where

-- Routing types
data RouteNowhere    = RouteNowhere
data RouteDone a     = RouteDone a | 
                       RouteDoneNowhere
data RouteFirst r    = RouteFirst r |
                       RouteFirstNowhere
data RouteSecond r   = RouteSecond r |
                       RouteSecondNowhere
data RouteBoth r1 r2 = RouteBoth r1 r2 |
                       RouteOnlyFirst r1 |
                       RouteOnlySecond r2 |
                       RouteNeither
                       
                     
-- Signal vector types
data SVEmpty
data SVEvent a
data SVSignal a
data SVAppend v1 v2

-- Type families for routes
type family CombineRoutes r1 r2 :: *
type instance CombineRoutes RouteNowhere RouteNowhere = Nowhere
type instance CombineRoutes (RouteDone a) Nowhere = RouteFirst (RouteDone a)
type instance CombineRoutes (RouteFirst r) Nowhere = RouteFirst (RouteFirst r)
type instance CombineRoutes (RouteSecond r) Nowhere = RouteFirst (RouteSecond r)
type instance CombineRoutes (RouteBoth r1 r2) Nowhere = RouteFirst (RouteBoth r1 r2)
type instance CombineRoutes Nowhere (RouteDone a) = RouteSecond (RouteDone a)
type instance CombineRoutes Nowhere (RouteFirst r) = RouteSecond (RouteFirst r)
type instance CombineRoutes Nowhere (RouteSecond r) = RouteSecond (RouteSecond r)
type instance CombineRoutes Nowhere (RouteBoth r1 r2) = RouteSecond (RouteBoth r1 r2)
type instance CombineRoutes (RouteDone a) (RouteDone b) = RouteBoth (RouteDone a) (RouteDone b)
type instance CombineRoutes (RouteDone a) (RouteFirst r) = RouteBoth (RouteDone a) (RouteFirst r)
type instance CombineRoutes (RouteDone a) (RouteSecond r) = RouteBoth (RouteDone a) (RouteSecond r)
type instance CombineRoutes (RouteDone a) (RouteBoth r1 r2) = RouteBoth (RouteDone a) (RouteBoth r1 r2) 
type instance CombineRoutes (RouteFirst r1) (RouteDone b) = RouteBoth (RouteFirst r1) (RouteDone b)
type instance CombineRoutes (RouteFirst r1) (RouteFirst r) = RouteBoth (RouteFirst r1) (RouteFirst r2)
type instance CombineRoutes (RouteFirst r1) (RouteSecond r) = RouteBoth (RouteFirst r1) (RouteSecond r2)
type instance CombineRoutes (RouteFirst r1) (RouteBoth r2 r3) = RouteBoth (RouteFirst r1) (RouteBoth r2 r3) 
type instance CombineRoutes (RouteSecond r1) (RouteDone b) = RouteBoth (RouteSecond r1) (RouteDone b)
type instance CombineRoutes (RouteSecond r1) (RouteFirst r2) = RouteBoth (RouteSecond r1) (RouteFirst r2)
type instance CombineRoutes (RouteSecond r1) (RouteSecond r2) = RouteBoth (RouteSecond r1) (RouteSecond r2)
type instance CombineRoutes (RouteSecond r1) (RouteBoth r2 r3) = RouteBoth (RouteSecond r1) (RouteBoth r2 r3)
type instance CombineRoutes (RouteBoth r1 r2) (RouteDone b) = RouteBoth (RouteBoth r1 r2) (RouteDone b)
type instance CombineRoutes (RouteBoth r1 r2) (RouteFirst r3) = RouteBoth (RouteBoth r1 r2) (RouteFirst r3)
type instance CombineRoutes (RouteBoth r1 r2) (RouteSecond r3) = RouteBoth (RouteBoth r1 r2) (RouteSecond r3)
type instance CombineRoutes (RouteBoth r1 r2) (RouteBoth r3 r4) = RouteBoth (RouteBoth r1 r2) (RouteBoth r3 r4)

type family VectorEventRoute v :: *
type instance VectorEventRoute SVEmpty = Nowhere
type instance VectorEventRoute (SVSignal a) = Nowhere
type instance VectorEventRoute (SVEvent a) = Done a
type instance VectorEventRoute (AppendV v1 v2) = CombineRoutes (VectorEventRoute v1) (VectorEventRoute v2)


{-
-- Typeclass relating route types and signal vector types
class EventRouteForVector v r | v -> r

instance EventRouteForVector SVEmpty Nowhere
instance EventRouteForVector (Signal a) Nowhere
instance EventRouteForVector (Event a) (Done a)
instance (EventRouteForVector v1 Nowhere,
          EventRouteForVector v2 Nowhere) =>
         EventRouteForVector (SVAppend v1 v2) Nowhere
instance (EventRouteForVector v1 r1,
          EventRouteForVector v2 Nowhere) =>
         EventRouteForVector (SVAppend v1 v2) (RouteFirst r1)
instance (EventRouteForVector v1 Nowhere,
          EventRouteForVector v2 r2) =>
         EventRouteForVector (SVAppend v1 v2) (RouteSecond r2)
instance (EventRouteForVector v1 r1,
          EventRouteForVector v2 r2) =>
         EventRouteForVector (SVAppend v1 v2) (RouteBoth r1 r2)
-}
-- Typeclass for pushing events
class PushEvent rf v1 v2 r1 r2 | v1 -> r1, v2 -> r2 where
  pushEvent :: rf v1 v2 -> r1 -> (rf v1 v2, r2)
         
-- Wrapper for reactive functions
newtype RF v1 v2 = RF (forall rf .(PushEvent rf v1 v2 (VectorEventRoute v1) (VectorEventRoute v2)) => rf v1 v2)
                   
instance PushEvent RF v1 v2 (VectorEventRoute v1) (VectorEventRoute v2) where
  pushEvent (RF rf) r = let (rf', r') = pushEvent rf r
                        in (RF rf', r')
                          

data RFSignalPure v1 v2 where
  RFSignalPure :: (a -> b) -> RFSignalPure (Signal a) (Signal b)
  
data RFEventPure v1 v2 where 
  RFEventPure :: (a -> b) -> RFEventPure (Event a) (Event b)
  
data RFSequence v1 v2 where
  RFSequence :: RF v1 v2 -> RF v2 v3 -> RFSequence v1 v3
  
data RFPass v1 v2 where
  RFPass :: RF v1 v2 -> RFPass (SVAppend v1 v3) (SVAppend v2 v3)
  
data RFCopy v1 v2 where
  RFCopy :: RFCopy v1 v2
  
data RFSwap v1 v2 where
  RFSwap :: RFSwap (SVAppendV v1 v2) (SVAppend v2 v1)
  
instance PushEvent RFSignalPure (Signal a) (Signal b) RouteNowhere RouteNowhere where
  pushEvent sf Nowhere = (sf, Nowhere)
  
instance PushEvent RFEventPure (Event a) (Event b) (Done a) (Done b) where
  pushEvent ef@(RFEventPure f) (Done x) = (ef, Done (f x))
  
instance PushEvent RFSequence v1 v2 (VectorEventRoute v1) (VectorEventRoute v2) where
  pushEvent (RFSequence rf1 rf2) r = let (r', rf1') = pushEvent rf1 r
                                         (r'', rf2') = pushEvent rf2 r'
                                     in (RFSequence rf1' rf2', r'')

-- Utility classes for routing combinators
type family FirstSubRouteType r :: *
type instance FirstSubRouteType RouteNowhere = RouteNowhere     
type instance FirstSubRouteType (RouteFirst r) = r
type instance FirstSubRouteType (RouteSecond r) = RouteNowhere
type instance FirstSubRouteType (RouteBoth r1 r2) = r1

type family SecondSubRouteType r :: *
type instance SecondSubRouteType RouteNowhere = RouteNowhere
type instance SecondSubRouteType (RouteFirst r) = RouteNowhere
type instance SecondSubRouteType (RouteSecond r) = r
type instance SecondSubRouteType (RouteBoth r1 r2) = r2

class NowhereOf r where
  nowhereOf :: r
  
instance NowhereOf Nowhere where
 nowhereOf = RouteNowhere
 
instance NowhereOf (Done a) where
  nowhereOf = RouteDoneNowhere

instance NowhereOf (RouteFirst r) where
  nowhereOf = RouteFirstNowhere
  
instance NowhereOf (RouteSecond r) where
  nowhereOf = RouteSecondNowhere
  
instance NowhereOf (RouteBoth r1 r2) where
  nowhereOf = RouteNeither

class FirstSubRoute r where
  firstSubRoute :: r -> FirstSubRouteType r
  
instance FirstSubRoute RouteNowhere where
  firstSubRoute RouteNowhere = RouteNowhere

instance (NowhereOf r) => FirstSubRoute (RouteFirst r) where
  firstSubRoute RouteFirstNowhere = nowhereOf
  firstSubRoute RouteFirst r = r
  
instance (NowhereOf r) => FirstSubRoute (RouteSecond r) where
  firstSubRoute _ = nowhereOf
  
instance (NowhereOf r) => FirstSubRoute (RouteBoth r1 r2) where
  firstSubRoute (RouteBoth r _) = r
  firstSubRoute (RouteOnlyFirst r) = r
  firstSubRoute (RouteOnlySecond _) = nowhereOf
  firstSubRoute RouteNeither = nowhereOf

class SecondSubRoute r where
  secondSubRoute :: r -> SecondSubRouteType r
  
instance SecondSubRoute RouteNowhere where
  secondSubRoute RouteNowhere = RouteNowhere

instance (NowhereOf r) => SecondSubRoute (RouteSecond r) where
  secondSubRoute RouteSecondNowhere = nowhereOf
  secondSubRoute RouteSecond r = r
  
instance (NowhereOf r) => SecondSubRoute (RouteFirst r) where
  secondSubRoute _ = nowhereOf
  
instance (NowhereOf r) => SecondSubRoute (RouteBoth r1 r2) where
  secondSubRoute (RouteBoth _ r) = r
  secondSubRoute (RouteOnlySecond r) = r
  secondSubRoute (RouteOnlyFirst _) = nowhereOf
  secondSubRoute RouteNeither = nowhereOf
  
class CombineRoutesClass r1 r2 where
  combineRoutes :: r1 r2 -> CombineRoutes r1 r2
  
instance CombineRoutes RouteNowhere RouteNowhere where
  combineRoutes _ _ = RouteNowhere
  
instance CombineRoutes 

                                
instance PushEvent RFPass v1 v2 (VectorEventRoute v1) (VectorEventRoute v2) where
  pushEvent (RFPass 
  