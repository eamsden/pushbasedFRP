{-# LANGUAGE EmptyDataDecls, GADTs, KindSignatures #-}
module ReactiveVectors (
  -- * (Type level) Reactive vectors
  -- $reactivevectors 
  RVEmpty,
  RVSignal,
  RVEvent,
  RVAppend

  -- * Representations of reactive values
  -- ** Signal vector representation
  SignalMemory(..),
  signalMemoryLeft,
  signalMemoryRight,
  signalMemoryCombine,
  signalMemoryUpdate,

  -- ** Signal change representation
  SignalChange(..),
  splitChanges,

  -- ** Event occurrence representation
  EventOccurrence(..),
  splitOccurrences
) where
    

-- $reactivevectors
-- These are type level vectors of reactive elements, which describe
-- the inputs and outputs of reactive functions 

-- | Empty reactive vector
data RVEmpty

-- | Singleton signal reactive vector
data RVSignal a

-- | Singleton event reactive vector
data RVEvent a

-- | Combine two reactive vectors
data RVAppend v1 v2

-- | Signal vectors carry values for all signal elements in a corresponding type-level
-- reactive vector
data SignalMemory :: * -> * where
  SignalMemoryUnknown :: SignalMemory v1
  SignalMemoryValue   :: a -> SignalMemory (RVSignal a)
  SignalMemoryNode    :: SignalMemory v1 -> SignalMemory v2 -> SignalMemory v1 v2


data Change :: * -> * where
  ChangeSignal :: a -> ChangeValue (RVSignal a)
  ChangeEvent  :: a -> ChangeEvent (RVEvent a)
  ChangeLeft   :: Change v1 -> (ChangeLeft (RVAppend v1 v2))
  ChangeRight  :: Change v2 -> (ChangeRight (RVAppend v1 v2))

data SignalChange :: * -> * where
  ChangeValue :: a -> ChangeValue (RVSignal a)
  ChangeLeft  :: SignalChange v1 -> SignalChange (RVAppend v1 v2)
  ChangeRight :: SignalChange v2 -> SignalChange (RVAppend v1 v2)

-- | Event occurrences carry typechecked information about which event
--  element in a type-level reactive vector the occurence corresponds to
data EventOccurrence :: * -> * where
  -- | An event occurrence
  OValue :: a -> EventOccurrence (RVEvent a)

  -- | Route to the first element in a combined vector
  OLeft  :: EventOccurrence v1 -> EventOccurrence (RVAppend v1 v2)

  -- | Route to the second element in a combined vector
  ORight :: EventOccurrence v2 -> EventOccurrence (RVAppend v1 v2)

-- | The left side of a signal memory tree
signalMemoryLeft :: SignalMemory (RVAppend v1 v2) -> SignalMemory v1
signalMemoryLeft SignalMemoryUnknown = SignalMemoryUnknown
signalMemoryLeft (SignalMemoryNode left _) = left

-- | The right side of a signal memory tree
signalMemoryRight :: SignalMemory (RVAppend v1 v2) -> SignalMemory v2
signalMemoryRight SignalMemoryUnknown = SignalMemoryUnknown
signalMemoryRight (SignalMemoryNode _ right) = right

-- | Combine two signal memory trees
signalMemoryCombine :: SignalMemory v1 -> SignalMemory v2 -> SignalMemory (RVAppend v1 v2)
signalMemoryCombine SignalMemoryUnknown SignalMemoryUnknown = SignalMemoryUnknown
signalMemoryCombine left right = SignalMemoryNode left right

-- | Apply a change to a memory
signalMemoryUpdate :: SignalMemory v1 -> SignalChange v1 -> SignalMemory v1
signalMemoryUpdate SignalMemoryUnknown (ChangeLeft c) = SignalMemoryNode (signalMemoryUpdate SignalMemoryUnknown c) SignalMemoryUnknown
signalMemoryUpdate SignalMemoryUnknown (ChangeRight c) = SignalMemoryNode SignalMemoryUnknown (signalMemoryUpdate SignalMemoryUnknown c)
signalMemoryUpdate (SignalMemoryNode left right) (ChangeLeft c) = SignalMemoryNode (signalMemoryUpdate left c) right
signalMemoryUpdate (SignalMemoryNode left right) (ChangeRight c) = SignalMemoryNode left (signalMemoryUpdate right c)
signalMemoryUpdate _ (ChangeSignal x) = SignalMemoryValue x
signalMemoryUpdate s (ChangeEvent _) = s

-- | Split a list of changes by left and right
splitChanges :: [Change (RVAppend v1 v2)] -> ([Change v1],[Change v2])
splitChanges = foldr (\(lefts, rights) change -> case change of
                                                    ChangeLeft c -> (c:lefts, rights)
                                                    ChangeRight c -> (lefts, c:rights)) ([],[])

