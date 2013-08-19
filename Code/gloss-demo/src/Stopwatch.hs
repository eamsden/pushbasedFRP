{-# LANGUAGE TypeOperators #-}
module Stopwatch where

import FRP.TimeFlies.SignalFunctions

timeTracker :: Double -> (SVEvent a :~> SVSignal Double)
timeTracker initialInterval = switch $ uncancelLeft >>> 
                                       (second $ pureEventTransformer startTimeTracker) >>>
                                       (first $ constant initialInterval)
  where
    startTimeTracker :: a -> (SVEvent a :~> SVSignal Double)
    startTimeTracker _ = runningTimeTracker initialInterval

runningTimeTracker :: Double -> (SVEvent a :~> SVSignal Double)
runningTimeTracker initialTimeInterval = switch $ uncancelLeft >>> 
                                                  first (time >>> 
                                                         pureSignalTransformer (+ initialTimeInterval) >>> 
                                                         copy) >>>
                                                  associate >>>
                                                  second (capture >>> pureEventTransformer stopTimeTracker)
  where
    stopTimeTracker :: (a, Double) -> (SVEvent a :~> SVSignal Double)
    stopTimeTracker (_, t) = timeTracker t
                                     
