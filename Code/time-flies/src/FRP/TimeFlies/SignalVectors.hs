{-# LANGUAGE KindSignatures, EmptyDataDecls, GADTs, TypeFamilies #-}
-- | This module defines /signal vectors/, a type-level representation
-- of the input and output structure of signal functions, and value
-- representations typed with signal vectors for use in implementing
-- signal functions.
module FRP.TimeFlies.SignalVectors
  (
    -- * Type Representation of Signal Vectors
    -- | A signal vector is a type which describes
    -- the structure of the input or output of a signal
    -- function. Signal vectors do not have value members,
    -- but by representing signal functions and other implementation
    -- types using GADTs, we can include signal vectors in the types
    -- of signal functions and their implementations.
    SVEmpty(), SVSignal(), SVEvent(), SVAppend(),

    -- * Signal Vector Element Functors
    -- | These newtypes are used as higher-kinded type parameters
    -- to the constructors of signal vector indices and signal vector memories,
    -- which both apply the functor to each element of the signal vector
    Id(..),
    To(..),

    -- * Value Representations of Signal Vectors
    -- | These types are used in the implementation of signal functions
    -- to represent values for inputs and outputs. The first parameter
    -- to each type is higher-kinded, and is applied to the type at an index
    -- of a signal vector to produce the actual type contained in the 
    -- representation. This permits, for instance, the re-use of these
    -- representations for storing output handlers in the monadic evaluator.

    -- ** Signal Vector Indices
    -- | Signal vector indices are representations of individual values at
    -- a particular index of a signal vector. They take the type of the whole
    -- signal vector, while dynamically representing which index the value
    -- corresponds to, and statically ensuring that this value matches the type.
    -- This representation is used internally to the implementation of signal
    -- functions (module "SignalFunctions") and is not manipulated by users.

    -- *** Datatype
    SVIndex(..),

    -- *** Helper Functions
    indexIsEvent,
    indexIsSignal,
    splitIndices,

    -- ** Signal Memories
    -- | Signal memories store values for a subset of all indices in a signal
    -- vector. Signal memories are used in the implementation of 
    -- signal functions (module "SignalFunctions") but may also be manipulated
    -- by the user for the specification of properties corresponding
    -- to evaluated signal functions. For instance, the monadic evaluation
    -- interface uses signal memories to specify output handlers for the
    -- signal function being evaluated.
 
    -- *** Datatypes
    SMemory(..),

    -- *** Helper Functions
    emptySMemory,
    updateSMemory,
    updateWithSMemory,
    combineSignalMemory,
    toIndices,
    applySMTo,
    applySMToSM
  )
  where

-- | Empty signal vector type
data SVEmpty
-- | Singleton signal signal vector type
data SVSignal a
-- | Singleton event signal vector type
data SVEvent a
-- | Combine two signal vector types
data SVAppend svLeft svRight

-- | \"Identity\" type constructor, for use as a functor on the element
-- types of a signal vector
newtype Id a = Id a

-- | Newtype representing a function \"to\" a value of the first type parameter,
-- from a value of the second parameter. Essentially "flip" for the arrow type.
newtype To b a = To (a -> b) 

-- | Push a type constructor to the leaves of a signal vector
type family SVMap sv (p :: * -> *)
type instance SVMap SVEmpty p = SVEmpty
type instance SVMap (SVSignal a) p = SVSignal (p a)
type instance SVMap (SVEvent a) p = SVEvent (p a)
type instance SVMap (SVAppend svLeft svRight) p = SVAppend (SVMap svLeft p) (SVMap svRight p)

-- | A single value at an index of a signal vector
data SVIndex p a where
  SVISignal  :: p a -> SVIndex p (SVSignal a)
  SVIEvent   :: p a -> SVIndex p (SVEvent a)
  SVILeft    :: SVIndex p svl -> SVIndex p (SVAppend svl svr)
  SVIRight   :: SVIndex p svr -> SVIndex p (SVAppend svl svr)

-- | A partial collection of values at indices of a signal vector
data SMemory p a where
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

-- | Merge two signal memories, given priority 
updateWithSMemory :: SMemory p v -> SMemory p v -> SMemory p v
updateWithSMemory SMEmpty mem = mem
updateWithSMemory mem SMEmpty = mem
updateWithSMemory (SMBoth meml memr) (SMBoth meml' memr') = SMBoth (updateWithSMemory meml meml') (updateWithSMemory memr memr')
updateWithSMemory sms@(SMSignal _) _ = sms
updateWithSMemory sme@(SMEvent _) _ = sme

-- | Create a list of signal vector indices from a signal memory
toIndices :: SMemory p v -> [SVIndex p v]
toIndices SMEmpty = []
toIndices (SMSignal x) = [SVISignal x]
toIndices (SMEvent x) = [SVIEvent x]
toIndices (SMBoth sml smr) = map SVILeft (toIndices sml) ++ map SVIRight (toIndices smr)

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

-- | Split a list of indices into lists of left and right indices
splitIndices :: [SVIndex p (SVAppend svl svr)] -> ([SVIndex p svl], [SVIndex p svr])
splitIndices [] = ([], [])
splitIndices ((SVILeft x):idxs) = let (xs, ys) = splitIndices idxs
                                in (x:xs, ys)
splitIndices ((SVIRight y):idxs) = let (xs, ys) = splitIndices idxs
                                 in (xs, y:ys)

-- | Apply a signal memory holding functions to signal indices holding values
applySMTo :: SMemory (To a) sv -> [SVIndex Id sv] -> [a]
applySMTo (SMSignal (To f)) idxs = map (\(SVISignal (Id x)) -> f x) idxs
applySMTo (SMEvent (To f)) idxs = map (\(SVIEvent (Id x)) -> f x) idxs
applySMTo (SMBoth sml smr) idxs = let (leftidxs, rightidxs) = splitIndices idxs
                                  in applySMTo sml leftidxs ++ applySMTo smr rightidxs
applySMTo SMEmpty _ = []

-- | Apply a signal memory holding functions to a signal memory holding values,
-- producing a list of values.
applySMToSM :: SMemory (To a) sv -> SMemory Id sv -> [a]
applySMToSM _ SMEmpty = []
applySMToSM SMEmpty _ = []
applySMToSM (SMSignal (To f)) (SMSignal (Id x)) = [f x]
applySMToSM (SMEvent (To f)) (SMEvent (Id x)) = [f x]
applySMToSM (SMBoth sml smr) (SMBoth sml' smr') = applySMToSM sml sml' ++ applySMToSM smr smr'