{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, FlexibleInstances, FunctionalDependencies #-}
module ReactiveFunctions where

import ReactiveVectors

data RF v1 v2 where
  RFPureSignal :: (a -> b) -> RF (RVSignal a) (RVSignal b)
  RFPureEvent  :: (a -> b) -> RF (RVEvent a) (RVEvent b)
  RFSwitch     :: RF v1 (RVAppend v2 (RVEvent a)) -> (a -> RF v1 v2) -> RF v1 v2
  RFSequence   :: RF v1 v2 -> RF v2 v3 -> RF v1 v3
  RFPass       :: RF v1 v2 -> RF (RVAppend v1 v3) (RVAppend v2 v3)
  RFCancelL    :: RF (RVAppend RVEmpty v1) v1
  RFUncancelL  :: RF v1 (RVAppend RVEmpty v1)
  RFAssoc      :: RF (RVAppend (RVAppend v1 v2) v3) (RVAppend v1 (RVAppend v2 v3))
  RFUnassoc    :: RF (RVAppend v1 (RVAppend v2 v3)) (RVAppend (RVAppend v1 v2) v3)
  RFCopy       :: RF v1 (RVAppend v1 v1)
  RFDrop       :: RF v1 RVEmpty
  RFSwap       :: RF (RVAppend v1 v2) (RVAppend v2 v1)
  
-- Event pushing for reactive functions
pushEvent :: RF v1 v2 -> EventRoute v1 (EventRouteType v1) -> (RF v1 v2, EventRoute v2 (EventRouteType v2))
pushEvent sf@(RFPureSignal _) _ = (sf, EventRouteSignal)
pushEvent ef@(RFPureEvent _) EventRouteNoEvent = (ef, EventRouteNoEvent)
pushEvent ef@(RFPureEvent f) (EventRouteEvent x) = (ef, EventRouteEvent (f x))
pushEvent sw@(RFSwitch rf f) r = let (rf', r') = pushEvent rf r
                                 in case (eventRouteSecond r') of
                                     EventRouteEvent x -> (f x, eventRouteFirst r')
                                     EventRouteNoEvent -> (RFSwitch rf' f, eventRouteFirst r')
pushEvent (RFSequence rf1 rf2) r = let (rf1', r') = pushEvent rf1 r
                                       (rf2', r'') = pushEvent rf2 r'
                                   in (RFSequence rf1' rf2', r'')
pushEvent (RFPass rf) r = let (rf', rFirst') = pushEvent rf (eventRouteFirst r)
                          in (RFPass rf', combineEventRoute rFirst' (eventRouteSecond r))
pushEvent RFCancelL r = (RFCancelL, eventRouteSecond r)
pushEvent RFUncancelL r = (RFUncancelL, combineEventRoute EventRouteEmpty r)
pushEvent RFAssoc r = (RFAssoc, let rp = eventRouteFirst r
                                    r1 = eventRouteFirst rp
                                    r2 = eventRouteSecond rp
                                    r3 = eventRouteSecond r
                                in combineEventRoute r1 (combineEventRoute r2 r3))
pushEvent RFUnassoc r = (RFUnassoc, let r1 = eventRouteFirst r
                                        rp = eventRouteSecond r
                                        r2 = eventRouteFirst rp
                                        r3 = eventRouteSecond rp
                                    in combineEventRoute (combineEventRoute r1 r2) r3)
pushEvent RFCopy r = (RFCopy, combineEventRoute r r)
pushEvent RFDrop _ = (RFDrop, EventRouteEmpty)
pushEvent RFSwap r = (RFSwap, combineEventRoute (eventRouteSecond r) (eventRouteFirst r)) 




