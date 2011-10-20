{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Routes where

-- Event Routes
data EventRouteNowhere    = EventRouteNowhere

data EventRouteDone a     = EventRouteDone a | 
                            EventRouteDoneNowhere
                            
data EventRouteFirst r    = EventRouteFirst r |
                            EventRouteFirstNowhere
                            
data EventRouteSecond r   = EventRouteSecond r | 
                            EventRouteSecondNowhere
                            
data EventRouteBoth r1 r2 = EventRouteBoth r1 r2 |
                            EventRouteOnlyFirst r1 |
                            EventRouteOnlySecond r2 |
                            EventRouteNeither
                            
-- Event Route Combination
class EventRoutesCombine r1 r2 where
  type CombineEventRoutes r1 r2
  combineEventRoutes :: r1 -> r2 -> CombineEventRoutes r1 r2
  eventRouteFirst :: CombineEventRoutes r1 r2 -> r1
  eventRouteSecond :: CombineEventRoutes r1 r2 -> r2
  
instance EventRoutesCombine EventRouteNowhere EventRouteNowhere where
  type CombineEventRoutes EventRouteNowhere EventRouteNowhere = EventRouteNowhere
  combineEventRoutes _ _ = EventRouteNowhere
  eventRouteFirst _ = EventRouteNowhere
  eventRouteSecond _ = EventRouteNowhere
  
instance EventRoutesCombine EventRouteNowhere (EventRouteDone a) where
  type CombineEventRoutes EventRouteNowhere (EventRouteDone a) = EventRouteSecond (EventRouteDone a)
  combineEventRoutes _ EventRouteDoneNowhere = EventRouteSecondNowhere
  combineEventRoutes _ r = EventRouteSecond r
  eventRouteFirst _ = EventRouteNowhere
  eventRouteSecond (EventRouteSecond r) = r
  eventRouteSecond _ = EventRouteDoneNowhere
  
instance EventRoutesCombine EventRouteNowhere (EventRouteFirst r) where
  type CombineEventRoutes EventRouteNowhere (EventRouteFirst r) = EventRouteSecond (EventRouteFirst r)
  combineEventRoutes _ EventRouteFirstNowhere = EventRouteSecondNowhere
  combineEventRoutes _ r = EventRouteSecond r
  eventRouteFirst _ = EventRouteNowhere
  eventRouteSecond (EventRouteSecond r) = r
  eventRouteSecond _ = EventRouteFirstNowhere
  
instance EventRoutesCombine EventRouteNowhere (EventRouteSecond r) where
  type CombineEventRoutes EventRouteNowhere (EventRouteSecond r) = EventRouteSecond (EventRouteSecond r)
  combineEventRoutes _ EventRouteSecondNowhere = EventRouteSecondNowhere
  combineEventRoutes _ r = EventRouteSecond r
  eventRouteFirst _ = EventRouteNowhere
  eventRouteSecond (EventRouteSecond r) = r
  eventRouteSecond _ = EventRouteSecondNowhere

instance EventRoutesCombine EventRouteNowhere (EventRouteBoth r1 r2) where
  type CombineEventRoutes EventRouteNowhere (EventRouteBoth r1 r2) = EventRouteSecond (EventRouteBoth r1 r2)
  combineEventRoutes _ EventRouteNeither = EventRouteSecondNowhere
  combineEventRoutes _ r = EventRouteSecond r
  eventRouteFirst _ = EventRouteNowhere
  eventRouteSecond (EventRouteSecond r) = r
  eventRouteSecond _ = EventRouteNeither
  
instance EventRoutesCombine (EventRouteDone a) EventRouteNowhere where
  type CombineEventRoutes (EventRouteDone a) EventRouteNowhere = EventRouteFirst (EventRouteDone a)
  combineEventRoutes EventRouteDoneNowhere _ = EventRouteFirstNowhere
  combineEventRoutes r _ = EventRouteFirst r
  eventRouteFirst (EventRouteFirst r) = r
  eventRouteFirst _ = EventRouteDoneNowhere
  eventRouteSecond _ = EventRouteNowhere
  
instance EventRoutesCombine (EventRouteFirst r) EventRouteNowhere where
  type CombineEventRoutes (EventRouteFirst r) EventRouteNowhere = EventRouteFirst (EventRouteFirst r)
  combineEventRoutes EventRouteFirstNowhere _ = EventRouteFirstNowhere
  combineEventRoutes r _ = EventRouteFirst r
  eventRouteFirst (EventRouteFirst r) = r
  eventRouteFirst _ = EventRouteFirstNowhere
  eventRouteSecond _ = EventRouteNowhere
  
instance EventRoutesCombine (EventRouteSecond r) EventRouteNowhere where
  type CombineEventRoutes (EventRouteSecond r) EventRouteNowhere = EventRouteFirst (EventRouteSecond r)
  combineEventRoutes EventRouteSecondNowhere _ = EventRouteFirstNowhere
  combineEventRoutes r _ = EventRouteFirst r
  eventRouteFirst (EventRouteFirst r) = r
  eventRouteFirst _ = EventRouteSecondNowhere
  eventRouteSecond _ = EventRouteNowhere

  
instance EventRoutesCombine (EventRouteBoth r1 r2) EventRouteNowhere where
  type CombineEventRoutes (EventRouteBoth r1 r2) EventRouteNowhere = EventRouteFirst (EventRouteBoth r1 r2)
  combineEventRoutes EventRouteNeither _ = EventRouteFirstNowhere
  combineEventRoutes r _ = EventRouteFirst r
  eventRouteFirst (EventRouteFirst r) = r
  eventRouteFirst _ = EventRouteNeither
  eventRouteSecond _ = EventRouteNowhere

  
instance EventRoutesCombine (EventRouteDone a) (EventRouteDone b) where
  type CombineEventRoutes (EventRouteDone a) (EventRouteDone b) = EventRouteBoth (EventRouteDone a) (EventRouteDone b)
  combineEventRoutes EventRouteDoneNowhere EventRouteDoneNowhere = EventRouteNeither
  combineEventRoutes r EventRouteDoneNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteDoneNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteDoneNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteDoneNowhere
  
instance EventRoutesCombine (EventRouteDone a) (EventRouteFirst r) where
  type CombineEventRoutes (EventRouteDone a) (EventRouteFirst r) = EventRouteBoth (EventRouteDone a) (EventRouteFirst r)
  combineEventRoutes EventRouteDoneNowhere EventRouteFirstNowhere = EventRouteNeither
  combineEventRoutes r EventRouteFirstNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteDoneNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteDoneNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteFirstNowhere
  
instance EventRoutesCombine (EventRouteDone a) (EventRouteSecond r) where
  type CombineEventRoutes (EventRouteDone a) (EventRouteSecond r) = EventRouteBoth (EventRouteDone a) (EventRouteSecond r)
  combineEventRoutes EventRouteDoneNowhere EventRouteSecondNowhere = EventRouteNeither
  combineEventRoutes r EventRouteSecondNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteDoneNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteDoneNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteSecondNowhere
  

instance EventRoutesCombine (EventRouteDone a) (EventRouteBoth r1 r2) where
  type CombineEventRoutes (EventRouteDone a) (EventRouteBoth r1 r2) = EventRouteBoth (EventRouteDone a) (EventRouteBoth r1 r2)
  combineEventRoutes EventRouteDoneNowhere EventRouteNeither = EventRouteNeither
  combineEventRoutes r EventRouteNeither = EventRouteOnlyFirst r
  combineEventRoutes EventRouteDoneNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteDoneNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteNeither

instance EventRoutesCombine (EventRouteFirst r) (EventRouteDone b) where
  type CombineEventRoutes (EventRouteFirst r) (EventRouteDone b) = EventRouteBoth (EventRouteFirst r) (EventRouteDone b)
  combineEventRoutes EventRouteFirstNowhere EventRouteDoneNowhere = EventRouteNeither
  combineEventRoutes r EventRouteDoneNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteFirstNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteFirstNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteDoneNowhere
  
instance EventRoutesCombine (EventRouteFirst r1) (EventRouteFirst r2) where
  type CombineEventRoutes (EventRouteFirst r1) (EventRouteFirst r2) = EventRouteBoth (EventRouteFirst r1) (EventRouteFirst r2)
  combineEventRoutes EventRouteFirstNowhere EventRouteFirstNowhere = EventRouteNeither
  combineEventRoutes r EventRouteFirstNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteFirstNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteFirstNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteFirstNowhere
  
instance EventRoutesCombine (EventRouteFirst r1) (EventRouteSecond r2) where
  type CombineEventRoutes (EventRouteFirst r1) (EventRouteSecond r2) = EventRouteBoth (EventRouteFirst r1) (EventRouteSecond r2)
  combineEventRoutes EventRouteFirstNowhere EventRouteSecondNowhere = EventRouteNeither
  combineEventRoutes r EventRouteSecondNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteFirstNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteFirstNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteSecondNowhere
  
instance EventRoutesCombine (EventRouteFirst r) (EventRouteBoth r1 r2) where
  type CombineEventRoutes (EventRouteFirst r) (EventRouteBoth r1 r2) = EventRouteBoth (EventRouteFirst r) (EventRouteBoth r1 r2)
  combineEventRoutes EventRouteFirstNowhere EventRouteNeither = EventRouteNeither
  combineEventRoutes r EventRouteNeither = EventRouteOnlyFirst r
  combineEventRoutes EventRouteFirstNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteFirstNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteNeither
  
instance EventRoutesCombine (EventRouteSecond r) (EventRouteDone b) where
  type CombineEventRoutes (EventRouteSecond r) (EventRouteDone b) = EventRouteBoth (EventRouteSecond r) (EventRouteDone b)
  combineEventRoutes EventRouteSecondNowhere EventRouteDoneNowhere = EventRouteNeither
  combineEventRoutes r EventRouteDoneNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteSecondNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteSecondNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteDoneNowhere
    
instance EventRoutesCombine (EventRouteSecond r1) (EventRouteFirst r2) where
  type CombineEventRoutes (EventRouteSecond r1) (EventRouteFirst r2) = EventRouteBoth (EventRouteSecond r1) (EventRouteFirst r2)
  combineEventRoutes EventRouteSecondNowhere EventRouteFirstNowhere = EventRouteNeither
  combineEventRoutes r EventRouteFirstNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteSecondNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteSecondNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteFirstNowhere
  
instance EventRoutesCombine (EventRouteSecond r1) (EventRouteSecond r2) where
  type CombineEventRoutes (EventRouteSecond r1) (EventRouteSecond r2) = EventRouteBoth (EventRouteSecond r1) (EventRouteSecond r2)
  combineEventRoutes EventRouteSecondNowhere EventRouteSecondNowhere = EventRouteNeither
  combineEventRoutes r EventRouteSecondNowhere = EventRouteOnlyFirst r
  combineEventRoutes EventRouteSecondNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteSecondNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteSecondNowhere
  
instance EventRoutesCombine (EventRouteSecond r) (EventRouteBoth r1 r2) where
  type CombineEventRoutes (EventRouteSecond r) (EventRouteBoth r1 r2) = EventRouteBoth (EventRouteSecond r) (EventRouteBoth r1 r2)
  combineEventRoutes EventRouteSecondNowhere EventRouteNeither = EventRouteNeither
  combineEventRoutes r EventRouteNeither = EventRouteOnlyFirst r
  combineEventRoutes EventRouteSecondNowhere r = EventRouteOnlySecond r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteSecondNowhere
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteNeither
  
instance EventRoutesCombine (EventRouteBoth r1 r2) (EventRouteDone a) where
  type CombineEventRoutes (EventRouteBoth r1 r2) (EventRouteDone a) = EventRouteBoth (EventRouteBoth r1 r2) (EventRouteDone a)
  combineEventRoutes EventRouteNeither EventRouteDoneNowhere = EventRouteNeither
  combineEventRoutes EventRouteNeither r = EventRouteOnlySecond r
  combineEventRoutes r EventRouteDoneNowhere = EventRouteOnlyFirst r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteNeither
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteDoneNowhere
  
instance EventRoutesCombine (EventRouteBoth r1 r2) (EventRouteFirst r3) where
  type CombineEventRoutes (EventRouteBoth r1 r2) (EventRouteFirst r3) = EventRouteBoth (EventRouteBoth r1 r2) (EventRouteFirst r3)
  combineEventRoutes EventRouteNeither EventRouteFirstNowhere = EventRouteNeither
  combineEventRoutes EventRouteNeither r = EventRouteOnlySecond r
  combineEventRoutes r EventRouteFirstNowhere = EventRouteOnlyFirst r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteNeither
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteFirstNowhere
  
instance EventRoutesCombine (EventRouteBoth r1 r2) (EventRouteSecond r3) where
  type CombineEventRoutes (EventRouteBoth r1 r2) (EventRouteSecond r3) = EventRouteBoth (EventRouteBoth r1 r2) (EventRouteSecond r3)
  combineEventRoutes EventRouteNeither EventRouteSecondNowhere = EventRouteNeither
  combineEventRoutes EventRouteNeither r = EventRouteOnlySecond r
  combineEventRoutes r EventRouteSecondNowhere = EventRouteOnlyFirst r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteNeither
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteSecondNowhere
 
instance EventRoutesCombine (EventRouteBoth r1 r2) (EventRouteBoth r3 r4) where
  type CombineEventRoutes (EventRouteBoth r1 r2) (EventRouteBoth r3 r4) = EventRouteBoth (EventRouteBoth r1 r2) (EventRouteBoth r3 r4)
  combineEventRoutes EventRouteNeither EventRouteNeither = EventRouteNeither
  combineEventRoutes EventRouteNeither r = EventRouteOnlySecond r
  combineEventRoutes r EventRouteNeither = EventRouteOnlyFirst r
  combineEventRoutes r1 r2 = EventRouteBoth r1 r2
  eventRouteFirst (EventRouteBoth r1 _) = r1
  eventRouteFirst (EventRouteOnlyFirst r1) = r1
  eventRouteFirst _ = EventRouteNeither
  eventRouteSecond (EventRouteBoth _ r2) = r2
  eventRouteSecond (EventRouteOnlySecond r2) = r2
  eventRouteSecond _ = EventRouteNeither

