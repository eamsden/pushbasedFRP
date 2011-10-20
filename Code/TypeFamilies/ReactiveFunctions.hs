{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, FlexibleInstances, FunctionalDependencies #-}
module ReactiveFunctions where

import Routes
import ReactiveVectors

-- Event pushing
class PushEvent rf v1 v2 
  pushEvent :: rf v1 v2 -> EventRoute v1 -> (rf v1 v2, EventRoute v2)
  
-- Wrapper type for reactive functions
data RF v1 v2 = forall c . PushEvent c v1 v2 => RF (c v1 v2)

instance PushEvent RF v1 v2 where
  pushEvent (RF f) r = let (f', r') = pushEvent f r 
                       in (RF f', r')

-- Reactive Function constructors
data RFPureSignal v1 v2 where
  RFPureSignal :: (a -> b) -> RFPureSignal (SVSignal a) (SVSignal b)
  
data RFPureEvent v1 v2 where
  RFPureEvent :: (a -> b) -> RFPureEvent (SVEvent a) (SVEvent b)
  
data RFSequence v1 v2 where
  RFSequence :: RF v1 v2 -> RF v2 v3 -> RFSequence v1 v3
  
data RFCopy v1 v2 where
  RFCopy :: RFCopy v1 (SVAppend v1 v1)

  
-- Event pushing for reactive functions
instance PushEvent RFPureSignal (SVSignal a) (SVSignal b) (EventRoute (SVSignal a)) (EventRoute (SVSignal b))where
  pushEvent sf _ = (sf, EventRouteNowhere)
  
instance PushEvent RFPureEvent (SVEvent a) (SVEvent b) where
  pushEvent ef EventRouteDoneNowhere = (ef, EventRouteDoneNowhere)
  pushEvent ef@(RFPureEvent f) (EventRouteDone x) = (ef, EventRouteDone (f x))
  
instance PushEvent RFSequence v1 v2 where
  pushEvent (RFSequence rf1 rf2) r = let (rf1', r') = pushEvent rf1 r
                                         (rf2', r'') = pushEvent rf2 r'
                                     in (RFSequence rf1' rf2', r'')
{-                                        
-- Fails with "no instance for EventRoutesCombine (EventRoute v1) (EventRoute v1)"
instance PushEvent RFCopy v1 (SVAppend v1 v1) where
  pushEvent rf r = (rf, combineEventRoutes r r)
-}
  
