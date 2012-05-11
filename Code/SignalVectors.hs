{-# LANGUAGE KindSignatures, EmptyDataDecls, GADTs #-}
module SignalVectors
  (
    SVEmpty(), SVSignal(), SVEvent(), SVAppend(),
    Id(..),
    SVIndex(..),
    SMemory(..),
    emptySMemory,
    updateSMemory,
    indexIsEvent,
    indexIsSignal,
    combineSignalMemory,
    splitIndices
  )
  where

-- Signal Vectors
-- | Empty signal vector phantom type
data SVEmpty  :: *
-- | Singleton signal signal vector phantom type
data SVSignal :: * -> *
-- | Singleton event signal vector phantom type
data SVEvent  :: * -> *
-- | Combine two signal vector phantom types
data SVAppend :: * -> * -> *

-- | "Identity" type constructor
newtype Id a = Id a

-- | Signal Vector indices
data SVIndex :: (* -> *) -> * -> * where
  SVISignal  :: p a -> SVIndex p (SVSignal a)
  SVIEvent   :: p a -> SVIndex p (SVEvent a)
  SVILeft    :: SVIndex p svl -> SVIndex p (SVAppend svl svr)
  SVIRight   :: SVIndex p svr -> SVIndex p (SVAppend svl svr)

-- | Signal memories
data SMemory :: (* -> *) -> * -> * where
  SMEmpty  :: SMemory p sv
  SMSignal :: p a -> SMemory p (SVSignal a)
  SMEvent  :: p a -> SMemory p (SVEvent a)
  SMBoth   :: SMemory p svl -> SMemory p svr -> SMemory p (SVAppend svl svr)

-- | An empty signal memory
emptySMemory :: SMemory p sv
emptySMemory = SMEmpty

-- | Update signal memory with a signal vector index
updateSMemory :: SMemory p sv -> SVIndex p sv -> SMemory p sv
updateSMemory _ (SVISignal x) = SMSignal x
updateSMemory _ (SVIEvent x) = SMEvent x
updateSMemory (SMBoth memL memR) (SVILeft sig) = SMBoth (updateSMemory memL sig) memR
updateSMemory (SMBoth memL memR) (SVIRight sig) = SMBoth memL (updateSMemory memR sig)

-- | Combine two signal memories
combineSignalMemory :: SMemory p vl -> SMemory p vr -> SMemory p (SVAppend vl vr)
combineSignalMemory SMEmpty SMEmpty = SMEmpty
combineSignalMemory SMEmpty x = SMBoth SMEmpty x
combineSignalMemory x y = SMBoth x y

-- | Is this particular index to an event?
indexIsEvent :: SVIndex p sv -> Bool
indexIsEvent (SVISignal _)  = False
indexIsEvent (SVIEvent _)   = True
indexIsEvent (SVILeft idx)  = indexIsEvent idx
indexIsEvent (SVIRight idx) = indexIsEvent idx

-- | Is this particular index to a signal?
indexIsSignal :: SVIndex p sv -> Bool
indexIsSignal (SVISignal _)  = True
indexIsSignal (SVIEvent _)   = False
indexIsSignal (SVILeft idx)  = indexIsSignal idx
indexIsSignal (SVIRight idx) = indexIsSignal idx

-- | Split a list of indices
splitIndices :: [SVIndex p (SVAppend svl svr)] -> ([SVIndex p svl], [SVIndex p svr])
splitIndices [] = ([], [])
splitIndices ((SVILeft x):idxs) = let (xs, ys) = splitIndices idxs
                                in (x:xs, ys)
splitIndices ((SVIRight y):idxs) = let (xs, ys) = splitIndices idxs
                                 in (xs, y:ys)