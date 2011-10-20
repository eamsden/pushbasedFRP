{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, RankNTypes #-}

module Marrow where

import Control.Monad
import Control.Monad.State.Lazy
import Data.HList

data Event l a
data Signal l a

data NoLabel

class Reactive a
instance Reactive Event
instance Reactive Signal

-- We don't care about labels for equivalent signal vectors
class SignalVectorEquivalent v1 v2
instance SignalVectorEquivalent HNil HNil
instance (SignalVectorEquivalent v1 v2) => SignalVectorEquivalent (HCons (Event l1 a) v1) (HCons (Event l2 a) v2)

-- Reactive vectors: lists of reactive types
class (HList a) => ReactiveVector a
instance ReactiveVector HNil
instance (Reactive r, ReactiveVector v) = ReactiveVector (HCons (r l a) v)

-- Event vectors: lists of event-only types
class EventVector a
instance EventVector HNil
instance (EventVector v) => EventVector (HCons (Event l a) v)

-- Signal vectors
class SignalVector a
instance SignalVector HNil
instance (SignalVector v) => SignalVector (HCons (Event l a) v)

-- Only the event pieces of an HList
class (HList l, HList l') => EventsOf l l' | l -> l'
instance EventsOf HNil HNil
instance (EventsOf l l') => EventsOf (HCons (Event l a) l) (HCons (Event l a) l')
instance (EventsOf l l') => EventsOf (HCons (Signal l a) l) l'

-- Only the signal pieces of an HList
class (HList l, HList l') => SignalsOf l l' | l -> l'
instance SignalsOf HNil HNil
instance (SignalOf l l') => SignalsOf (HCons (Signal l a) l) (HCons (Signal l a) l')
instance (SignalOf l l') => SignalsOf (HCons (Event l a) l) l'


-- Reactive Function type
data (ReactiveVector v1, ReactiveVector v2) => ReactiveFunction v1 v2 where
  -- Pure functions
  PureSignal :: (SignalVector v1, SignalVector v2, TypeVector v1 t1, TypeVector t2 v2) => (t1 -> t2) -> ReactiveFunction v1 v2

  
type Reactive v1 v2 a = ReactiveT v1 v2 Identity a

type ReactiveIO v1 v2 a = ReactiveT v1 v2 IO a


