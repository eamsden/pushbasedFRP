{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, FlexibleInstances, FunctionalDependencies #-}
module ReactiveFunctions where

import ReactiveVectors

data RF v1 v2 where
  RFPureSignal  :: (a -> b) -> RF (RVSignal a) (RVSignal b)
  RFConst       :: a -> RF RVEmpty (RVSignal a)
  RFSignalAccum :: acc -> (acc -> a -> (acc, b)) -> RF (RVSignal a) (RVSignal b)
  RFTime        :: Double -> RF RVEmpty (RVSignal Double)
  RFPureEvent   :: (a -> b) -> RF (RVEvent a) (RVEvent b)
  RFPredicate   :: (a -> Maybe b) -> RF (RVSignal a) (RVEvent b)
  RFSwitch      :: RF v1 (RVAppend v2 (RVEvent a)) -> (a -> RF v1 v2) -> RF v1 v2
  RFSequence    :: RF v1 v2 -> RF v2 v3 -> RF v1 v3
  RFPass        :: RF v1 v2 -> RF (RVAppend v1 v3) (RVAppend v2 v3)
  RFCancelL     :: RF (RVAppend RVEmpty v1) v1
  RFUncancelL   :: RF v1 (RVAppend RVEmpty v1)
  RFAssoc       :: RF (RVAppend (RVAppend v1 v2) v3) (RVAppend v1 (RVAppend v2 v3))
  RFUnassoc     :: RF (RVAppend v1 (RVAppend v2 v3)) (RVAppend (RVAppend v1 v2) v3)
  RFCopy        :: RF v1 (RVAppend v1 v1)
  RFDrop        :: RF v1 RVEmpty
  RFSwap        :: RF (RVAppend v1 v2) (RVAppend v2 v1)
  
-- Event pushing for reactive functions
pushEvent :: RF v1 v2 -> EventRoute v1 (EventRouteType v1) -> (RF v1 v2, EventRoute v2 (EventRouteType v2))
pushEvent sf@(RFPureSignal _) _ = (sf, EventRouteSignal)
pushEvent sf@(RFConst _) _ = (sf, EventRouteSignal)
pushEvent sf@(RFSignalAccum _ _) _ = (sf, EventRouteSignal)
pushEvent sf@(RFTime _) _ = (sf, EventRouteSignal)
pushEvent ef@(RFPureEvent _) EventRouteNoEvent = (ef, EventRouteNoEvent)
pushEvent ef@(RFPureEvent f) (EventRouteEvent x) = (ef, EventRouteEvent (f x))
pushEvent ef@(RFPredicate _) _ = (ef, EventRouteNoEvent)
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


-- Time stepping
-- TODO: rewrite to use pushEvent, to avoid duplicating work/differing semantics
-- (semantics for switch?)
timeStep :: RF v1 v2 -> Double -> SignalRoute v1 (SignalRouteType v1) -> (RF v1 v2, SignalRoute v2 (SignalRouteType v2))
timeStep sf@(RFPureSignal f) _ (SignalRouteSignal x) = (sf, SignalRouteSignal (f x))
timeStep sf@(RFConst x) _ _ = (sf, SignalRouteSignal x) 
timeStep (RFSignalAccum a f) _ (SignalRouteSignal x) = let (a', x') = f a x 
                                                 in (RFSignalAccum a' f, SignalRouteSignal x')
timeStep (RFTime t) dt _ = let t' = t + dt
                           in (RFTime t', SignalRouteSignal t')
timeStep sf@(RFPureEvent f) _ (SignalRouteEvent x) = (sf, SignalRouteEvent (f x))
timeStep sf@(RFPureEvent f) _ SignalRouteNoEvent = (sf, SignalRouteNoEvent)
timeStep sf@(RFPredicate f) _ (SignalRouteSignal x) = (sf, (case f x of
                                                               Just x -> SignalRouteEvent x
                                                               Nothing -> SignalRouteNoEvent))
timeStep sf@(RFSwitch rf f) dt r = let (rf', r') = timeStep rf dt r
                                   in (case signalRouteSecond r' of
                                          (SignalRouteEvent x) -> (f x, signalRouteFirst r')
                                          (SignalRouteNoEvent) -> (sf, signalRouteFirst r'))
timeStep (RFSequence rf1 rf2) dt r = let (rf1', r') = timeStep rf1 dt r
                                         (rf2', r'') = timeStep rf2 dt r'
                                     in (RFSequence rf1' rf2', r'')
timeStep (RFPass rf) dt r = let rFirst = signalRouteFirst r
                                rSecond = signalRouteSecond r
                                (rf', rFirst') = timeStep rf dt rFirst
                            in (RFPass rf', combineSignalRoute rFirst' rSecond)
timeStep sf@RFCancelL _ r = (sf, signalRouteSecond r)
timeStep sf@RFUncancelL _ r = (sf, combineSignalRoute SignalRouteEmpty r)
timeStep sf@RFAssoc _ r = let rp = signalRouteFirst r
                              r1 = signalRouteFirst rp
                              r2 = signalRouteSecond rp
                              r3 = signalRouteSecond r
                          in (sf, combineSignalRoute r1 (combineSignalRoute r2 r3))
timeStep sf@RFUnassoc _ r = let r1 = signalRouteFirst r
                                rp = signalRouteSecond r
                                r2 = signalRouteFirst rp
                                r3 = signalRouteSecond rp
                            in (sf, combineSignalRoute (combineSignalRoute r1 r2) r3)

                                      
timeStep sf@RFCopy _ r = (sf, combineSignalRoute r r)
timeStep sf@RFDrop _ _ = (sf, SignalRouteEmpty)
timeStep sf@RFSwap _ r = (sf, combineSignalRoute (signalRouteSecond r) (signalRouteFirst r))


