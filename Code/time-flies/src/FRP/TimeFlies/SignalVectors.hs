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

    -- * Representations of Signal Vectors
    -- | Signal vectors abstract away from the representation,
    -- permitting different structures to be used to reify
    -- the vector depending on the need.

    -- ** Event Representation
    -- | This type and associated functions represent occurrences of events in
    -- a signal vector, and allow these representations to be constructed,
    -- deconstructed, and queried
    SVOccurrence(), occurrence, occLeft, occRight,
    splitOccurrences, occurrenceListToMaybe, fromOccurrence, chooseOccurrence,
    
    -- ** Signal Representations
    -- | These types give full and partial representations of the
    -- signals in a vector. An "SVSample" is intended for initializing
    -- signal functions, and storing signal state where necessary.
    -- An "SVDelta" is used to pass changes in a signal between signal functions.
    SVSample(), sample, sampleEvt, sampleNothing, combineSamples, splitSample,
    sampleValue, sampleDelta,
    SVDelta(), updateSample, delta, deltaNothing, combineDeltas, splitDelta,
    deltaValue,

    -- ** Handler vectors
    -- | An "SVHandler" is a collection of functions from the leaf types
    -- of a signal vector to a type specified in the handler. This is used
    -- for specifying output handlers in evaluation
    SVHandler(), emptyHandler, eventHandler, signalHandler, combineHandlers,
    applyHandlerOccurrence, applyHandlerDelta,

    -- * Types for describing evaluation inputs
    -- | These types are used to describe the inputs to a signal function
    -- within the evaluation interface.
    SVRoutable(..), SVEventInput(), svOcc, inputToOccurrence, SVSignalUpdate(), svSig, updateDelta
) where

-- | Empty signal vector type
data SVEmpty
-- | Singleton signal signal vector type
data SVSignal a
-- | Singleton event signal vector type
data SVEvent a
-- | Combine two signal vector types
data SVAppend svLeft svRight

-- | Event representation
data SVOccurrence sv where
  SVOccurrence :: a -> SVOccurrence (SVEvent a)
  SVOccLeft    :: SVOccurrence svLeft -> SVOccurrence (SVAppend svLeft svRight)
  SVOccRight   :: SVOccurrence svRight -> SVOccurrence (SVAppend svLeft svRight)

-- | Construct an occurrence
occurrence :: a -> SVOccurrence (SVEvent a)
occurrence = SVOccurrence

-- | Send an occurrence left
occLeft :: SVOccurrence svLeft -> SVOccurrence (SVAppend svLeft svRight)
occLeft = SVOccLeft

-- | Send an occurrence right
occRight :: SVOccurrence svRight -> SVOccurrence (SVAppend svLeft svRight)
occRight = SVOccRight

-- | Split a list of occurrences into lists of left and right
splitOccurrences :: [SVOccurrence (SVAppend svLeft svRight)] -> ([SVOccurrence svLeft], [SVOccurrence svRight])
splitOccurrences = foldl (\(leftList, rightList) occurrence -> case occurrence of
                                                                 SVOccLeft occurrence' -> (occurrence':leftList, rightList)
                                                                 SVOccRight occurrence' -> (leftList, occurrence':rightList))
                   ([], [])

-- | Give back the value of the first occurrence, or nothing if the occurrence list is empty
occurrenceListToMaybe :: [SVOccurrence (SVEvent a)] -> Maybe a
occurrenceListToMaybe ((SVOccurrence x):_) = Just x
occurrenceListToMaybe _ = Nothing

-- | The value of the occurrence
fromOccurrence :: SVOccurrence (SVEvent a) -> a
fromOccurrence (SVOccurrence x) = x

-- | Allow case matching on an occurrence
chooseOccurrence :: SVOccurrence (SVAppend svLeft svRight) -> Either (SVOccurrence svLeft) (SVOccurrence svRight)
chooseOccurrence (SVOccLeft occLeft) = Left occLeft
chooseOccurrence (SVOccRight occRight) = Right occRight

-- | Full signal sample representation
data SVSample sv where
  SVSample      :: a -> SVSample (SVSignal a)
  SVSampleEvent :: SVSample (SVEvent a)
  SVSampleEmpty :: SVSample SVEmpty
  SVSampleBoth  :: SVSample svLeft -> SVSample svRight -> SVSample (SVAppend svLeft svRight)

-- | Construct a signal sample
sample :: a -> SVSample (SVSignal a)
sample = SVSample

-- | An event placeholder in a sample
sampleEvt :: SVSample (SVEvent a)
sampleEvt = SVSampleEvent

-- | An empty placeholder in a sample
sampleNothing :: SVSample SVEmpty
sampleNothing = SVSampleEmpty

-- | Combine samples
combineSamples :: SVSample svLeft -> SVSample svRight -> SVSample (SVAppend svLeft svRight)
combineSamples = SVSampleBoth

-- | Split a sample
splitSample :: SVSample (SVAppend svLeft svRight) -> (SVSample svLeft, SVSample svRight)
splitSample (SVSampleBoth sampleLeft sampleRight) = (sampleLeft, sampleRight)

-- | The value of a sample
sampleValue :: SVSample (SVSignal a) -> a
sampleValue (SVSample x) = x

-- | Create a delta from a sample
sampleDelta :: SVSample sv -> SVDelta sv
sampleDelta SVSampleEmpty = SVDeltaNothing
sampleDelta SVSampleEvent = SVDeltaNothing
sampleDelta (SVSample x) = SVDeltaSignal x
sampleDelta (SVSampleBoth sampleLeft sampleRight) = combineDeltas (sampleDelta sampleLeft) (sampleDelta sampleRight)

-- | Signal delta representation
data SVDelta sv where
  SVDeltaSignal  :: a -> SVDelta (SVSignal a)
  SVDeltaNothing :: SVDelta sv
  SVDeltaBoth    :: SVDelta svLeft -> SVDelta svRight -> SVDelta (SVAppend svLeft svRight)

-- | Use a signal delta to update a signal sample
updateSample :: SVSample sv -> SVDelta sv -> SVSample sv
updateSample sample SVDeltaNothing = sample
updateSample (SVSampleBoth sampleLeft sampleRight) (SVDeltaBoth deltaLeft deltaRight) = SVSampleBoth (updateSample sampleLeft deltaLeft) (updateSample sampleRight deltaRight)
updateSample _ (SVDeltaSignal x) = SVSample x

-- | An empty signal delta
deltaNothing :: SVDelta sv
deltaNothing = SVDeltaNothing

-- | A signal delta
delta :: a -> SVDelta (SVSignal a)
delta = SVDeltaSignal

-- | Combine signal deltas
combineDeltas :: SVDelta svLeft -> SVDelta svRight -> SVDelta (SVAppend svLeft svRight)
combineDeltas SVDeltaNothing SVDeltaNothing = SVDeltaNothing
combineDeltas deltaLeft deltaRight = SVDeltaBoth deltaLeft deltaRight

-- | Split a signal delta
splitDelta :: SVDelta (SVAppend svLeft svRight) -> (SVDelta svLeft, SVDelta svRight)
splitDelta SVDeltaNothing = (SVDeltaNothing, SVDeltaNothing)
splitDelta (SVDeltaBoth deltaLeft deltaRight) = (deltaLeft, deltaRight)

-- | The value of a signal delta
deltaValue :: SVDelta (SVSignal a) -> Maybe a
deltaValue SVDeltaNothing = Nothing
deltaValue (SVDeltaSignal x) = Just x

-- | A vector of handlers
data SVHandler out sv where
  SVHandlerEmpty  :: SVHandler out SVEmpty
  SVHandlerSignal :: (a -> out) -> SVHandler out (SVSignal a)
  SVHandlerEvent  :: (a -> out) -> SVHandler out (SVEvent a)
  SVHandlerBoth   :: SVHandler out svLeft -> SVHandler out svRight -> SVHandler out (SVAppend svLeft svRight)

-- | A handler for an empty vector
emptyHandler :: SVHandler out SVEmpty
emptyHandler = SVHandlerEmpty

-- | A handler for a signal sample
signalHandler :: (a -> out) -> SVHandler out (SVSignal a)
signalHandler = SVHandlerSignal

-- | A handler for event occurrences
eventHandler :: (a -> out) -> SVHandler out (SVEvent a)
eventHandler = SVHandlerEvent

-- | Combine handlers
combineHandlers :: SVHandler out svLeft -> SVHandler out svRight -> SVHandler out (SVAppend svLeft svRight)
combineHandlers = SVHandlerBoth

-- | Apply a handler to an event occurrence
applyHandlerOccurrence :: SVHandler out sv -> SVOccurrence sv -> out
applyHandlerOccurrence (SVHandlerEvent f) (SVOccurrence x) = f x
applyHandlerOccurrence (SVHandlerBoth handlerLeft _) (SVOccLeft occLeft) = applyHandlerOccurrence handlerLeft occLeft
applyHandlerOccurrence (SVHandlerBoth _ handlerRight) (SVOccRight occRight) = applyHandlerOccurrence handlerRight occRight

-- | Apply a handler to a signal delta
applyHandlerDelta :: SVHandler out sv -> SVDelta sv -> [out]
applyHandlerDelta _ SVDeltaNothing = []
applyHandlerDelta (SVHandlerSignal f) (SVDeltaSignal x) = [f x]
applyHandlerDelta (SVHandlerBoth handlerLeft handlerRight) (SVDeltaBoth signalLeft signalRight) = applyHandlerDelta handlerLeft signalLeft ++ applyHandlerDelta handlerRight signalRight

-- | Inputs which may be sent left or right
class SVRoutable (r :: * -> *) where
  svLeft  :: r svLeft -> r (SVAppend svLeft svRight)
  svRight :: r svRight -> r (SVAppend svLeft svRight)

-- | An event input
data SVEventInput sv where
  SVEOcc   :: a -> SVEventInput (SVEvent a)
  SVELeft  :: SVEventInput svLeft -> SVEventInput (SVAppend svLeft svRight)
  SVERight :: SVEventInput svRight -> SVEventInput (SVAppend svLeft svRight)

instance SVRoutable SVEventInput where
  svLeft = SVELeft
  svRight = SVERight

-- | Construct an event input
svOcc :: a -> SVEventInput (SVEvent a)
svOcc = SVEOcc

-- | Convert an event input to an event occurrence
inputToOccurrence :: SVEventInput sv -> SVOccurrence sv
inputToOccurrence (SVEOcc x) = SVOccurrence x
inputToOccurrence (SVELeft inp) = SVOccLeft (inputToOccurrence inp)
inputToOccurrence (SVERight inp) = SVOccRight (inputToOccurrence inp)

-- | A signal update
data SVSignalUpdate sv where
  SVUSig   :: a -> SVSignalUpdate (SVSignal a)
  SVULeft  :: SVSignalUpdate svLeft -> SVSignalUpdate (SVAppend svLeft svRight)
  SVURight :: SVSignalUpdate svRight -> SVSignalUpdate (SVAppend svLeft svRight)

instance SVRoutable SVSignalUpdate where
  svLeft = SVULeft
  svRight = SVURight

-- | Construct a signal update
svSig :: a -> SVSignalUpdate (SVSignal a)
svSig = SVUSig

-- Update a signal delta with a signal update
updateDelta :: SVDelta sv -> SVSignalUpdate sv -> SVDelta sv
updateDelta _ (SVUSig x) = SVDeltaSignal x
updateDelta (SVDeltaBoth deltaLeft deltaRight) (SVULeft update) = SVDeltaBoth (updateDelta deltaLeft update) deltaRight
updateDelta (SVDeltaBoth deltaLeft deltaRight) (SVURight update) = SVDeltaBoth deltaLeft (updateDelta deltaRight update)
updateDelta SVDeltaNothing (SVULeft update) = SVDeltaBoth (updateDelta SVDeltaNothing update) SVDeltaNothing
updateDelta SVDeltaNothing (SVURight update) = SVDeltaBoth SVDeltaNothing (updateDelta SVDeltaNothing update)

