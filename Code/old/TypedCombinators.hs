{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyDataDecls, GADTs #-}
module TypedCombinators where

data Signal a
data Event a
data Empty
data AppendV v1 v2

-- Reifying dataflow in the type system
data Nowhere = Nowhere
data Done a = Done a | DoneNowhere
data First r = First r | FirstNowhere
data Second r = Second r | SecondNowhere
data Both r1 r2 = Both r1 r2 | FirstOnly r1 | SecondOnly r2 | BothNowhere

class Router r
instance Router (Nowhere)
instance Router (Done a)
instance (Router r) => Router (First r)
instance (Router r) => Router (Second r)
instance (Router r1, Router r2) => Router (Both r1 r2)

class SubRouter r1 r2 where
  weakenRoute :: r1 -> r2
instance SubRouter Nowhere (Done a) where
  weakenRoute _ = DoneNowhere
instance SubRouter Nowhere Nowhere where
  weakenRoute _ = Nowhere
instance SubRouter Nowhere (First r) where
  weakenRoute _ = FirstNowhere
instance SubRouter Nowhere (Second r) where
  weakenRoute _ = SecondNowhere
instance SubRouter Nowhere (Both r1 r2) where
  weakenRoute _ = BothNowhere
instance SubRouter (Done a) (Done a) where
  weakenRoute = id
instance (SubRouter r1 r2) => SubRouter (First r1) (First r2) where
  weakenRoute (First r) = First (weakenRoute r)
  weakenRoute FirstNowhere = FirstNowhere
instance (SubRouter r1 r2) => SubRouter (Second r1) (Second r2) where
  weakenRoute (Second r) = Second (weakenRoute r)
  weakenRoute SecondNowhere = SecondNowhere
instance (SubRouter r1 r3) => SubRouter (Second r1) (Both r2 r3) where
  weakenRoute (Second r) = SecondOnly (weakenRoute r2)
  weakenRoute SecondNowhere = BothNowhere
instance (SubRouter r1 r2) => SubRouter (First r1) (Both r2 r3) where
  weakenRoute (First r)= FirstOnly (weakenRoute r)
  weakenRoute (FirstNowhere) = BothNowhere
instance (SubRouter r1 r3, SubRouter r2 r4) => SubRouter (Both r1 r2) (Both r3 r4) where
  weakenRoute (Both r1 r2) = Both (weakenRoute r1) (weakenRoute r2)
  weakenRoute BothNowhere = BothNowhere



-- Event pushing
class PushEvent sf r1 r2 | r2 -> r1 where
  pushEvent :: r1 -> sf -> (r2, sf)

instance PushEvent (EventFunction (Event a) (Event b)) (Done a) (Done b) where
  pushEvent (Done x) sf@(EventFunction f) = (Done (f x), sf)  
  
instance (SubRouter r2 r3,  PushEvent (sf1 v1 v2) r1 r2, PushEvent (sf2 v2 v3) r3 r4) => 
         PushEvent (Sequence (sf1 v1 v2) (sf2 v2 v3) v1 v3) r1 r4 where
  pushEvent r (Sequence sf1 sf2) = let (r', sf1') = pushEvent r sf1
                                       (r'', sf2') = pushEvent (weakenRoute r') sf2
                                   in (r'', Sequence sf1' sf2') 

instance PushEvent (Swap v1 v2) Nowhere Nowhere where
  pushEvent Nowhere sf = (Nowhere, sf)

instance PushEvent (Swap v1 v2) (First r) (Second r) where
  pushEvent (First r) sf = (Second r, sf)
  pushEvent FirstNowhere sf = (SecondNowhere, sf)
  
instance PushEvent (Swap v1 v2) (Second r) (First r) where
  pushEvent (Second r) sf = (First r, sf)
  pushEvent SecondNowhere sf = (FirstNowhere, sf)
  
instance PushEvent (Swap v1 v2) (Both r1 r2) (Both r2 r1) where
  pushEvent (Both r1 r2) sf = (Both r2 r1, sf)
  pushEvent (FirstOnly r) sf = (SecondOnly r, sf)
  pushEvent (SecondOnly r) sf = (FirstOnly r, sf)
  pushEvent BothNowhere sf = (BothNowhere, sf)
  
instance PushEvent (Pass (c1 v1 v2) (AppendV v1 v3) (AppendV v2 v3)) (Second r) (Second r) where
  pushEvent (Second r) sf = (Second r, sf)
  pushEvent SecondNowhere sf = (SecondNowhere, sf)
  
instance (PushEvent (c1 v1 v2) r1 r2) => 
         PushEvent (Pass (c1 v1 v2) (AppendV v1 v3) (AppendV v2 v3)) (First r1) (First r2) where
  pushEvent (First r) (Pass sf) = let (r', sf') = pushEvent r sf
                                  in (r', Pass sf')

instance (PushEvent (c1 v1 v2) r1 r2) => 
         PushEvent (Pass (c1 v1 v2) (AppendV v1 v3) (AppendV v2 v3)) (Both r1 r3) (Both r2 r3) where
  pushEvent (Both r1 r2) (Pass sf) = let (r1', sf') = pushEvent r1 sf
                                     in (Both r1' r2, Pass sf')
  pushEvent (FirstOnly r1) (Pass sf) = let (r1', sf') = pushEvent r1 sf
                                       in (FirstOnly r1', Pass sf')
  pushEvent (SecondOnly r2) (Pass sf) = (SecondOnly r2, Pass sf)
  pushEvent (BothNowhere) (Pass sf) = (BothNowhere, Pass sf)

instance (PushEvent (c1 v1 v2) r1 Nowhere) => 
         PushEvent (Pass (c1 v1 v2) (AppendV v1 v3) (AppendV v2 v3)) (Both r1 r3) (Second r3) where
  pushEvent (Both r1 r3) (Pass sf) = let (_, sf') = pushEvent r1 sf'
                                     in (Second r3, sf')
  pushEvent (FirstOnly r1) (Pass sf) = let (_, sf') = pushEvent r1 sf
                                       in (SecondNowhere, Pass sf')
  pushEvent (SecondOnly r2) (Pass sf) = (Second r2, Pass sf)
  pushEvent BothNowhere (Pass sf) = (SecondNowhere, Pass sf)
  
-- ...

data SignalFunction v1 v2 where
  SignalFunction :: 
    (a -> b) -> SignalFunction (Signal a) (Signal b)
  
data EventFunction v1 v2 where
  EventFunction :: 
    (a -> b) -> EventFunction (Event a) (Event b)
  
data Sequence sf1 sf2 v1 v2 where
  Sequence :: 
    c1 v1 v2 -> c2 v2 v3 -> Sequence (c1 v1 v2) (c2 v2 v3) v1 v3
  
data Pass sf v1 v2 where 
  Pass :: 
    c1 v1 v2 -> Pass (c1 v1 v2) (AppendV v1 v3) (AppendV v2 v3)
  
data Swap v1 v2 where
  Swap :: 
    Swap (AppendV v1 v2) (AppendV v2 v1)
  
data Copy v1 v2 where
  Copy :: 
    Copy v1 (AppendV v1 v1)
    
data Switch sf1 v1 v2 where
  Switch :: 
    c1 v1 v2 -> (a -> c1 v1 v2) -> Switch (AppendVector v1 (Event a)) v2
  

