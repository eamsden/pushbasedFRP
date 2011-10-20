{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies, RankNTypes #-}
module SignalFunction where

-- | An empty type vector
data EmptyVector
-- | A single signal
data Signal a
-- | A single event
data Event a
-- | The concatenation of two vectors
data AppendVector v1 v2

{-
instance ReactiveVector EmptyVector
instance ReactiveVector (Signal a)
instance ReactiveVector (Event a)
instance (ReactiveVector v1, ReactiveVector v2) => ReactiveVector (AppendVector v1 v2)
instance Singleton (Signal a)
instance Singleton (Event a)
instance SignalVector EmptyVector
instance SignalVector (Signal a)
instance (SignalVector v1, SignalVector v2) => SignalVector (AppendVector v1 v2)
instance EventVector EmptyVector
instance EventVector (Event a)
instance (EventVector v1, EventVector v2) => EventVector (AppendVector v1 v2)

-- | The head of a type level reactive vector
class VectorHead v e | v -> e
instance VectorHead (Signal a) (Signal a)
instance VectorHead (Event a) (Event a)
instance (VectorHead v e) => VectorHead (AppendVector v v') e 
instance (VectorHead v e) => VectorHead (AppendVector EmptyVector v) e

-- | The tail of a type level reactive vector
class VectorTail v vt | v -> vt 
instance VectorTail (Signal a) EmptyVector
instance VectorTail (Event a) EmptyVector
instance VectorTail (AppendVector (Signal a) v2) v2
instance VectorTail (AppendVector (Event a) v2 ) v2
instance (VectorTail v2 vt) => VectorTail (AppendVector EmptyVector v2) vt
instance (VectorTail v1 vt) => VectorTail (AppendVector v1 v2) (AppendVector vt v2)

-- | Equivalence test for two vectors
class (ReactiveVector v1, ReactiveVector v2) => VectorEquivalent v1 v2
instance VectorEquivalent (EmptyVector) (EmptyVector)
instance VectorEquivalent (Signal a) (Signal a)
instance VectorEquivalent (Event a) (Event a)
instance (ReactiveVector v1, ReactiveVector v2, VectorHead v1 h1, VectorHead v2 h1, 
          VectorTail v1 t1, VectorTail v2 t2, 
          VectorEquivalent t1 t2, VectorEquivalent h1 h2) => 
         VectorEquivalent v1 v2
-}

-- Pattern:
-- A ReactiveFunction is either ReactiveTerminator or
-- it takes as its last constructor parameter the reactive function to sequence itself with.

 
-- | Definition of reactive functions
data ReactiveFunction v1 v2 where
  -- | "Terminator"
  ReactiveTerminator :: ReactiveFunction v1 v1
  -- | Stateless signal transformer
  ReactivePureSignal :: (a -> b) -> ReactiveFunction (Signal b) v2 -> ReactiveFunction (Signal a) v2
  -- | Stateless event transformer
  ReactivePureEvent :: (PushEvent v2 b) => (a -> b) -> ReactiveFunction (Event b) v2 -> ReactiveFunction (Event a) v2
  -- | Reactivity to events
  --   Parameters: The initial function to act as, and the function to sequence with
  ReactiveSwitch :: ReactiveFunction v1 v2 -> ReactiveFunction v2 v3 -> ReactiveFunction (AppendVector v1 (Event (ReactiveFunction v1 v2))) v3
  -- | Event primitive
  --   Parameters: The time to event occurrence, and the function to act as
  ReactiveAfter :: Double -> a -> ReactiveFunction (Event a) v2 -> ReactiveFunction EmptyVector v2
  -- | Time primitive
  ReactiveTime :: ReactiveFunction (Signal Double) v2 -> ReactiveFunction EmptyVector v2
  -- | Primitive for defining stateful signals such as integration
  ReactiveSignalAccum :: (a -> b -> (c, b)) -> b -> ReactiveFunction (Signal c) v2 -> ReactiveFunction (Signal a) v2
  -- | GArrow Identity
  ReactiveIdentity :: ReactiveFunction v1 v2 -> ReactiveFunction v1 v2
 
  
  -- | Drop (not exposed)
  ReactiveDrop :: ReactiveFunction v1 
  -- | GArrow swap
  ReactiveSwap :: ReactiveFunction (AppendVector v2 v1) v3 -> ReactiveFunction (AppendVector v1 v2) v3
  -- | GArrow looping (use swap for left case)
  ReactiveLoopRight :: ReactiveFunction (AppendVector v1 vl) (AppendVector v2 vl) -> ReactiveFunction v2 v3 -> ReactiveFunction v1 v3

instance PushEvent (Event a) a where
  pushEvent x rf = case rf of
    ReactiveTerminator -> ReactiveTerminator
    ReactivePureEvent pf rf -> ReactivePureEvent pf $ pushEvent (pf x) rf
    ReactiveCopy rf = pushEvent x rf
    ReactiveUncancelL rf = pushEvent x rf
    
instance PushEvent v1 a => PushEvent (AppendVector v1 v1) a where
  pushEvent x rf = case rf of
    ReactiveTerminator -> ReactiveTerminator



