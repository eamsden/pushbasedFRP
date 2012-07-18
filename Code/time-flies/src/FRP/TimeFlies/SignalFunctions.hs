{-# LANGUAGE KindSignatures, EmptyDataDecls, GADTs, ScopedTypeVariables, NoMonomorphismRestriction, GeneralizedNewtypeDeriving, TypeOperators, TupleSections #-}

-- | Signal functions are stateful, reactive, and time-dependent entities
-- which respond to inputs by producing outputs. Signal functions are typed
-- using signal vectors, exported by the "SignalVectors" module. A signal
-- vector is formed from some combination of signals, events, and empty indices.
-- Signal inputs update only at time steps, while events may have occurrences
-- at any time, including between time steps. Event occurrences form a preorder
-- with respect to indicated occurrence times, (the reflexivity condition
-- is not satisfied).
--
-- To program with signal functions, primitive signal functions may be combined
-- using the routing combinators to connect inputs and outputs, as well as the
-- reactivity combinator 'switch' to permit signal functions to change behavior,
-- and the feedback combinator 'loop' to permit signal functions to respond
-- to their own output.
--
-- In this documentation the term /given signal function(s)/ refers to
-- signal functions passed as parameters to signal function combinators,
-- while (in that context) /signal function/ will refer to the signal function
-- produced by a combinator.

module FRP.TimeFlies.SignalFunctions (
  -- * Datatypes
  SF,
  NonInitialized,
  Initialized,
  (:~>),
  (:^:),
  SVEmpty,
  SVSignal,
  SVEvent,
  SVAppend,
  -- * Basic signal functions
  identity,
  constant,
  asap,
  after,
  never,
  -- * Lifting
  pureSignalTransformer,
  pureEventTransformer,
  -- * Composition and Routing
  (>>>),
  first,
  second,
  swap,
  copy,
  ignore,
  cancelLeft,
  uncancelLeft,
  cancelRight,
  uncancelRight,
  associate,
  unassociate,
  -- * Reactivity
  switch,
  -- * Feedback
  loop,
  -- * Time dependence
  time,
  delay,
  TimeIntegrate,
  integrate,
  -- * Joining
  union,
  combineSignals,
  capture,
  -- * Evaluation
  svLeft, svRight, svOcc, svSig,
  emptyHandler, eventHandler, signalHandler, combineHandlers,
  sample, sampleEvt, sampleNothing, combineSamples,
  SVEventInput(), SVSignalUpdate(),
  SFEvalT,
  SFEvalIO,
  SFEvalState,
  runSFEvalT,
  initSFEval,
  push,
  update,
  step
  
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Either (lefts, rights, either)
import Data.List (sortBy)
import Data.Maybe
import qualified Data.Tuple as T

import FRP.TimeFlies.SignalVectors

-- | Signal function running or suspended
data Initialized

-- | Signal function not yet running
data NonInitialized

-- | Signal functions: The first type parameter distinguishes between
-- uninitialized (just specificied) and initialized (ready to respond to input
-- and time) signal functions. The second and third type parameters
-- are the input and output signal vectors, respectively. 
data SF init svIn svOut where
  SF     :: (SVSample svIn -> (SVSample svOut, SF Initialized svIn svOut)) 
            -> SF NonInitialized svIn svOut
  SFInit :: (Double -> SVDelta svIn -> (SVDelta svOut, [SVOccurrence svOut], SF Initialized svIn svOut)) 
            -> (SVOccurrence svIn -> ([SVOccurrence svOut], SF Initialized svIn svOut)) 
            -> SF Initialized svIn svOut

-- | Infix type alias for signal functions
type svIn :~> svOut = SF NonInitialized svIn svOut

-- | Infix type alias for signal vectors
type svLeft :^: svRight = SVAppend svLeft svRight

-- Utility functions
-- | Apply a signal function to a list of changes, producing a list of
-- output changes and an updated signal function
applySF :: SF Initialized svIn svOut -> [SVOccurrence svIn] -> ([SVOccurrence svOut], SF Initialized svIn svOut)
applySF sf indices = foldr (\evtOcc (changes, SFInit _ changeCont) -> let (newChanges, nextSF) = changeCont evtOcc
                                                                      in (newChanges ++ changes, nextSF))
                     ([], sf)
                     indices

-- | Identity signal function: reproduce the input exactly as the output.
identity :: SF NonInitialized sv sv
identity = SF (\initSample -> (initSample, identityInit))

identityInit :: SF Initialized sv sv
identityInit = SFInit (\dt sigDelta -> (sigDelta, [], identityInit)) (\evtOcc -> ([evtOcc], identityInit))

-- | Constant signal function: Produce the signal
constant :: a -> SF NonInitialized SVEmpty (SVSignal a)
constant x = SF (\_ -> (sample x, constantInit))

constantInit :: SF Initialized SVEmpty (SVSignal a)
constantInit = SFInit (\_ _ -> (deltaNothing, [], constantInit))
                      (\_ -> ([], constantInit))

-- | Produce an event at the first time step after being switched in
asap :: a -> SF NonInitialized SVEmpty (SVEvent a)
asap x = SF (\_ -> (sampleEvt, asapInit x))

asapInit :: a -> SF Initialized SVEmpty (SVEvent a)
asapInit x = SFInit (\_ _ -> (deltaNothing, [occurrence x], neverInit))
                    (\_ -> ([], asapInit x))

-- | Never produce an event
never :: SF NonInitialized SVEmpty (SVEvent a)
never = SF (\_ -> (sampleEvt, neverInit))

neverInit :: SF Initialized SVEmpty (SVEvent a)
neverInit = SFInit (\_ _ -> (deltaNothing, [], neverInit))
                   (\_ -> ([], neverInit))

-- | Produce an event after some time
after :: Double -> a -> SF NonInitialized SVEmpty (SVEvent a)
after dt x = SF (\_ -> (sampleEvt, afterInit dt x))

afterInit :: Double -> a -> SF Initialized SVEmpty (SVEvent a)
afterInit dt x = SFInit (\dt' _ -> if dt' >= dt
                                   then (deltaNothing, [occurrence x], neverInit)
                                   else (deltaNothing, [], afterInit (dt - dt') x))
                        (\_ -> ([], afterInit dt x))
     

-- | Apply the given function to every sample of a signal
pureSignalTransformer :: (a -> b) -> SF NonInitialized (SVSignal a) (SVSignal b)
pureSignalTransformer f = SF ((, pureSignalTransformerInit f) . sample . f . sampleValue) 

pureSignalTransformerInit :: (a -> b) -> SF Initialized (SVSignal a) (SVSignal b)
pureSignalTransformerInit f = let psti = SFInit (flip (const . (, [], psti) . maybe deltaNothing delta . fmap f . deltaValue))
                                                (const ([], psti))
                              in psti

-- | Apply the given function to each occurrence of an event
pureEventTransformer :: (a -> b) -> SF NonInitialized (SVEvent a) (SVEvent b)
pureEventTransformer f = SF (const (sampleEvt, pureEventTransformerInit f))

pureEventTransformerInit :: (a -> b) -> SF Initialized (SVEvent a) (SVEvent b)
pureEventTransformerInit f = let peti = SFInit (const $ const (deltaNothing, [], peti))
                                               ((, peti) . (:[]) . occurrence . f . fromOccurrence)
                             in peti


-- | Produce a new signal function where the output of the first signal function is used as the input to the second
-- signal function
(>>>) :: SF NonInitialized svIn svBetween -> SF NonInitialized svBetween svOut -> SF NonInitialized svIn svOut
(SF sigSampleF1) >>> (SF sigSampleF2) = SF (\sigSample -> let (sigSample', sfInit1) = sigSampleF1 sigSample
                                                              (sigSample'', sfInit2) = sigSampleF2 sigSample'
                                                          in (sigSample'', composeInit sfInit1 sfInit2))

composeInit :: SF Initialized svIn svBetween -> SF Initialized svBetween svOut -> SF Initialized svIn svOut
composeInit (SFInit dtCont1 inputCont1) sf2@(SFInit dtCont2 inputCont2) =
  SFInit
    (\dt sigDelta -> let (sf1MemOutput, sf1EvtOutputs, sf1New) = dtCont1 dt sigDelta
                         (sf2MemOutput, sf2EvtOutputs, sf2New) = dtCont2 dt sf1MemOutput
                         (sf2EvtEvtOutputs, sf2Newest) = applySF sf2New sf1EvtOutputs
                     in (sf2MemOutput, sf2EvtOutputs ++ sf2EvtEvtOutputs, composeInit sf1New sf2Newest)
    )
    (\evtOcc -> let (sf1Outputs, newSf1) = inputCont1 evtOcc
                    (sf2FoldOutputs, newSf2) = applySF sf2 sf1Outputs
                in (sf2FoldOutputs, composeInit newSf1 newSf2)   
    )

-- | Produce a new signal function where the left side of the input is used as
-- input for the given signal function, and the right side of the input
-- is combined unchanged with the output of the given signal function
first :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend svIn sv) (SVAppend svOut sv)
first (SF sigSampleF) = SF (\sigSample -> let (leftSample, rightSample) = splitSample sigSample
                                              (leftSampleOut, sf) = sigSampleF leftSample
                                          in (combineSamples leftSampleOut rightSample, firstInit sf))

firstInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend svIn sv) (SVAppend svOut sv)
firstInit (SFInit timeCont inputCont) = 
  let firstInitSF = SFInit (\dt sigDelta -> let (input, rightOutput) = splitDelta sigDelta
                                                (sigDeltaOutput, evtOutput, sf1New) = timeCont dt input
                                            in (combineDeltas sigDeltaOutput rightOutput, map occLeft evtOutput, firstInit sf1New))
                           (\evtOcc -> case chooseOccurrence evtOcc of
                                         Left  lChange -> let (changes, sf) = inputCont lChange in (map occLeft changes, firstInit sf)
                                         Right rChange -> ([occRight rChange], firstInitSF))
  in firstInitSF

-- | Similar to 'first', except the right side of the signal vector is used
-- as input for the given signal function, and the left side is combined
-- unchanged with the output.
second :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend sv svIn) (SVAppend sv svOut)
second (SF sigSampleF) = SF (\sigSample -> let (leftSample, rightSample) = splitSample sigSample
                                               (rightSampleOut, sf) = sigSampleF rightSample
                                           in (combineSamples leftSample rightSampleOut, secondInit sf))

secondInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend sv svIn) (SVAppend sv svOut)
secondInit (SFInit timeCont inputCont) = 
  let secondInitSF = SFInit (\dt sigDelta -> let (leftDelta, rightDelta) = splitDelta sigDelta
                                                 (sigDeltaOutput, evtOutput, sf1New) = timeCont dt rightDelta
                                             in (combineDeltas leftDelta sigDeltaOutput , map occRight evtOutput, secondInit sf1New))
                            (\evtOcc -> case chooseOccurrence evtOcc of
                                          Right rChange -> let (changes, sf) = inputCont rChange in (map occRight changes, secondInit sf)
                                          Left lChange -> ([occLeft lChange], secondInitSF))
  in secondInitSF

-- | Produce the left side of the input as the right side of the output, 
-- and vice vers&#226;.
swap :: SF NonInitialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swap = SF ((, swapInit) . uncurry combineSamples . T.swap . splitSample)

swapInit :: SF Initialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swapInit = SFInit (flip (const . (, [], swapInit) . uncurry combineDeltas . T.swap . splitDelta))
                  (\evtOcc -> (case chooseOccurrence evtOcc of
                                 Left lOcc -> [occRight lOcc]
                                 Right rOcc -> [occLeft rOcc], swapInit))

-- | Produce the input as left and right sides of the output
copy :: SF NonInitialized sv (SVAppend sv sv)
copy = SF (\sigSample -> (combineSamples sigSample sigSample, copyInit))

copyInit :: SF Initialized sv (SVAppend sv sv)
copyInit = SFInit (\_ sigDelta -> (combineDeltas sigDelta sigDelta, [], copyInit)) 
                  (\evtOcc-> ([occLeft evtOcc, occRight evtOcc], copyInit))

-- | Discard the input entirely and produce an empty output.
ignore :: SF NonInitialized sv SVEmpty
ignore = SF (\_ -> (sampleNothing, ignoreInit))

ignoreInit :: SF Initialized sv SVEmpty 
ignoreInit = SFInit (const $ const (deltaNothing, [], ignoreInit)) (const ([], ignoreInit))

-- | Discard the empty left side of the input and use the right side as
-- output.
cancelLeft :: SF NonInitialized (SVAppend SVEmpty sv) sv
cancelLeft = SF ((, cancelLeftInit) . snd . splitSample)

cancelLeftInit :: SF Initialized (SVAppend SVEmpty sv) sv
cancelLeftInit = SFInit (flip $ const . (, [], cancelLeftInit) . snd . splitDelta)
                        ((, cancelLeftInit) . rights . (:[]) . chooseOccurrence )


-- | Use the input as the right side of the output and leave the left side empty
uncancelLeft :: SF NonInitialized sv (SVAppend SVEmpty sv)
uncancelLeft = SF ((, uncancelLeftInit) . combineSamples sampleNothing)

uncancelLeftInit :: SF Initialized sv (SVAppend SVEmpty sv)
uncancelLeftInit = SFInit (flip $ const . (, [], uncancelLeftInit) . combineDeltas deltaNothing) 
                          ((, uncancelLeftInit) . (:[]) . occRight)

-- | Discard the empty right side of the input and use the left side as the
-- output.
cancelRight :: SF NonInitialized (SVAppend sv SVEmpty) sv
cancelRight = SF ((, cancelRightInit) . fst . splitSample )

cancelRightInit :: SF Initialized (SVAppend sv SVEmpty) sv
cancelRightInit = SFInit (flip $ const . (, [], cancelRightInit) . fst . splitDelta )
                         ((, cancelRightInit) . lefts . (:[]) . chooseOccurrence)

-- | Use the input as the left side of the output and leave the right side empty
uncancelRight :: SF NonInitialized sv (SVAppend sv SVEmpty)
uncancelRight = SF ((, uncancelRightInit) . flip combineSamples sampleNothing)

uncancelRightInit :: SF Initialized sv (SVAppend sv SVEmpty)
uncancelRightInit = SFInit (flip $ const . (, [], uncancelRightInit) . flip combineDeltas deltaNothing)
                           ((, uncancelRightInit) . (:[]) . occLeft)

-- | Take an input where the left two subvectors are associated and 
-- produce an output with the same subvectors, but where the right
-- two subvectors are associated.
associate :: SF NonInitialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associate = SF (\sigSample -> let (sigSampleLeft, sigSampleRight) = splitSample sigSample
                                  (sigSampleLeftLeft, sigSampleLeftRight) = splitSample sigSampleLeft
                              in (combineSamples sigSampleLeftLeft (combineSamples sigSampleLeftRight sigSampleRight), associateInit))

associateInit :: SF Initialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associateInit = SFInit (\_ sigDelta -> let (sigDeltaLeft, sigDeltaRight) = splitDelta sigDelta
                                           (sigDeltaLeftLeft, sigDeltaLeftRight) = splitDelta sigDeltaLeft
                                       in (combineDeltas sigDeltaLeftLeft (combineDeltas sigDeltaLeftRight sigDeltaRight), [], associateInit)) 
                       (\evtOcc -> (case chooseOccurrence evtOcc of
                                      Left leftOcc -> case chooseOccurrence leftOcc of
                                                        Left leftLeftOcc -> [occLeft leftLeftOcc]
                                                        Right leftRightOcc -> [occRight $ occLeft leftRightOcc]
                                      Right rightOcc -> [occRight $ occRight $ rightOcc],
                                    associateInit))
         

-- | Take an input where the right two subvectors are associated and produce an
-- output with the same subvectors, but where the left two subvectors are
-- associated.
unassociate :: SF NonInitialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociate = SF (\sigSample -> let (sigSampleLeft, sigSampleRight) = splitSample sigSample
                                    (sigSampleRightLeft, sigSampleRightRight) = splitSample sigSampleRight
                                in (combineSamples (combineSamples sigSampleLeft sigSampleRightLeft) sigSampleRightRight, unassociateInit))

unassociateInit :: SF Initialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociateInit = SFInit (\_ sigDelta -> let (sigDeltaLeft, sigDeltaRight) = splitDelta sigDelta
                                             (sigDeltaRightLeft, sigDeltaRightRight) = splitDelta sigDeltaRight
                                         in (combineDeltas (combineDeltas sigDeltaLeft sigDeltaRightLeft) sigDeltaRightRight, [], unassociateInit))
                         (\evtOcc -> (case chooseOccurrence evtOcc of
                                        Left leftOcc -> [occLeft $ occLeft leftOcc]
                                        Right rightOcc -> case chooseOccurrence rightOcc of
                                                            Left rightLeftOcc -> [occLeft $ occRight rightLeftOcc]
                                                            Right rightRightOcc -> [occRight rightRightOcc],
                                      unassociateInit))

-- | Provide the input as input to the given signal function, producing
-- the left side of the given signal function's output as output. Upon
-- the first occurrence of the event on the right side of the given signal
-- function's output, act as the signal function contained by the occurrence.
-- Note that the if a switch event occurs in between time samples,
-- the switch function will combine the signal deltas produced by the initialization
-- of the new signal function and by the first time step of the new signal function.
switch :: SF NonInitialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF NonInitialized svIn svOut
switch (SF sigSampleF) = SF (\sigSample -> let (sigSampleSF, sf) = sigSampleF sigSample
                                               (sigSampleOut, _) = splitSample sigSampleSF
                                           in (sigSampleOut, switchInit sigSample sf))

switchInit :: SVSample svIn -> SF Initialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF Initialized svIn svOut
switchInit inputSample sf@(SFInit timeCont changeCont) =
  SFInit (\dt sigDelta -> let (sigDeltaSF, occsSF, newSF) = timeCont dt sigDelta
                              newInputSample = updateSample inputSample sigDelta
                              (sigDeltaOut, _) = splitDelta sigDeltaSF
                              (occsOut, occsSwitch) = splitOccurrences occsSF
                              (outputDelta, nextSF) = maybe (sigDeltaOut, switchInit newInputSample newSF)
                                                            (\(SF sigSampleF) -> let (outputSample, switchSF) = sigSampleF newInputSample
                                                                                     outputDelta = sampleDelta outputSample
                                                                                 in (outputDelta, switchSF))
                                                            $ occurrenceListToMaybe occsSwitch
                     in (outputDelta, occsOut, nextSF))
         (\evtOcc -> let (occsSF, newSF) = changeCont evtOcc
                         (occsOut, occsSwitch) = splitOccurrences occsSF
                         nextSF = maybe (switchInit inputSample newSF) 
                                        (\(SF sigSampleF) -> let (sampleOut, switchSF) = sigSampleF inputSample
                                                             in switchWait sampleOut switchSF)
                                        $ occurrenceListToMaybe occsSwitch
                     in (occsOut, nextSF))

switchWait :: SVSample svOut -> SF Initialized svIn svOut -> SF Initialized svIn svOut
switchWait outputSample sf@(SFInit timeCont changeCont) = SFInit (\dt sigDelta -> let (sigDeltaOut, outOccs, newSF) = timeCont dt sigDelta
                                                                                  in (sampleDelta $ updateSample outputSample sigDeltaOut, outOccs, newSF))
                                                                 (\evtOcc -> let (outOccs, newSF) = changeCont evtOcc
                                                                             in (outOccs, switchWait outputSample newSF))
                                    
-- | Provide the input as the left input to the given signal function. Produce
-- the left output of the given signal function as the output. Use the right
-- output of the given signal function as the right input to the given
-- signal function, thus providing a means for a signal function to act
-- on its own output and encode a feedback loop. This function is decoupled
-- for signals, meaning that a signal function will produce output that depends
-- on the signal input at the previous time step, but not for events, meaning
-- that feedback events will have an immediate effect. It is the programmer's
-- responsibility to ensure that an feedback event does not result in an
-- infinite set of resulting events.
loop :: SF NonInitialized (SVAppend svIn svLoop) (SVAppend svOut svLoop) -> SF NonInitialized svIn svOut
loop (SF sigSampleF) = SF (\sigSample -> let (sigSampleOut, sfInit) = sigSampleF (combineSamples sigSample sigSampleOutRight)
                                             (sigSampleOutLeft, sigSampleOutRight) = splitSample sigSampleOut
                                         in (sigSampleOutLeft, loopInit deltaNothing sfInit))

loopInit :: SVDelta svLoop -> SF Initialized (SVAppend svIn svLoop) (SVAppend svOut svLoop) -> SF Initialized svIn svOut
loopInit loopDelta (SFInit timeCont changeCont) = SFInit (\dt sigDelta -> let (sigDeltaOut, occsOut, newSF) = timeCont dt (combineDeltas sigDelta loopDelta)
                                                                              (occsOutput, loopOccs) = splitOccurrences occsOut
                                                                              (occsOutput', loopOccs') = splitOccurrences foldOccs
                                                                              (foldOccs, newSF') = applySF newSF (map occRight (loopOccs ++ loopOccs'))
                                                                              (outputDelta, newLoopDelta) = splitDelta sigDeltaOut
                                                                   in (outputDelta, occsOutput ++ occsOutput', loopInit newLoopDelta newSF'))
                                                       (\change -> let (occsOut, newSF) = changeCont $ occLeft change
                                                                       (occsOutput, loopOccs) = splitOccurrences occsOut
                                                                       (occsOutput', loopOccs') = splitOccurrences foldOccs
                                                                       (foldOccs, newSF') = applySF newSF (map occRight (loopOccs ++ loopOccs'))
                                                                   in (occsOutput ++ occsOutput', loopInit loopDelta newSF'))
                                                                       
                                          


-- | With an empty input, produce the time since the signal function began
-- running as a signal output.
time :: SF NonInitialized SVEmpty (SVSignal Double)
time = SF (\_ -> (sample 0, timeInit 0))

timeInit :: Double -> SF Initialized SVEmpty (SVSignal Double)
timeInit t = SFInit (\dt _ -> (delta (t + dt), [], timeInit (t + dt)))
                    (\_ -> ([], timeInit t))

-- | Maintain a time delay for events. The initial argument is the initial time
-- delay. Left input event occurrences are stored as state together with the 
-- time delay at occurrence time. After that delay has expired, a corresponding 
-- event will be produced on the output. Right input event occurrences update
-- the delay time. Note that this means that later input events may be output
-- before prior input events if the delay time is shortened in the intervening
-- time.

delay :: Double -> SF NonInitialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delay delayTime = SF (\_ -> (sampleEvt, delayInit [] delayTime 0))

delayInit :: [(Double, SVOccurrence (SVEvent a))] -> Double -> Double -> SF Initialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delayInit eventsBuffer delayTime time = SFInit (\dt _ -> let newTime = time + dt
                                                             (readyEvents, notReadyEvents) = break ((>= newTime) . fst) eventsBuffer
                                                         in (deltaNothing, map snd readyEvents, delayInit notReadyEvents delayTime newTime))
                                               (\evtOcc -> ([], case chooseOccurrence evtOcc of
                                                                  Left occLeft -> delayInit (sortBy (\(x,_ ) (y, _) -> compare x y) $ eventsBuffer ++ [(time + delayTime, occLeft)]) delayTime time
                                                                  Right occRight -> delayInit eventsBuffer (fromOccurrence occRight) time))
                                                            

-- | Class of values integrateable with respect to time
class TimeIntegrate i where
  iZero :: i
  iTimesDouble :: i -> Double -> i
  iPlus :: i -> i -> i
  iDifference :: i -> i -> i

instance TimeIntegrate Double where
  iZero = 0
  iTimesDouble = (*)
  iPlus = (+)
  iDifference = (-)


-- | Produce as output the rectangle rule integration of 
-- the input with respect to time.
integrate :: (TimeIntegrate i) => SF NonInitialized (SVSignal i) (SVSignal i)
integrate = SF (\sigSample -> let sValue = sampleValue sigSample
                              in (sample iZero, integrateInit iZero sValue))

integrateInit :: (TimeIntegrate i) => i -> i -> SF Initialized (SVSignal i) (SVSignal i)
integrateInit currentSum currentValue = SFInit (\dt sigDelta -> let newVal = maybe currentValue id $ deltaValue sigDelta
                                                                    newSum = currentSum `iPlus` (newVal `iTimesDouble` dt)
                                                                in (delta newSum, [], integrateInit newSum newVal))
                                               (\_ -> ([], integrateInit currentSum currentValue))

-- | Produce an event occurrence on the output corresponding to an event
-- occurrence on either input
union :: SF NonInitialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
union = SF (\_ -> (sampleEvt, unionInit))

unionInit :: SF Initialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
unionInit = SFInit (\dt _ -> (deltaNothing, [], unionInit)) 
                   (\evtOcc -> case chooseOccurrence evtOcc of
                                 Left lOcc -> ([lOcc], unionInit)
                                 Right rOcc -> ([rOcc], unionInit))

-- | Combine both inputs at each point in time using the given function.
combineSignals :: ((a, b) -> c) -> SF NonInitialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignals f = SF (\sigSample -> (sample $ f $ (\(l,r) -> (sampleValue l, sampleValue r)) $ splitSample sigSample, combineSignalsInit f sigSample))

combineSignalsInit :: ((a, b) -> c) -> SVSample (SVAppend (SVSignal a) (SVSignal b)) -> SF Initialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignalsInit f currentSample = SFInit (\dt sigDelta -> let newSample = updateSample currentSample sigDelta
                                                             in (delta $ f $ (\(l,r) -> (sampleValue l, sampleValue r)) $ splitSample newSample, [], combineSignalsInit f newSample))
                                            (const ([], combineSignalsInit f currentSample))


-- | Combine a signal and an event by producing an output event occurrence
-- for each input event occurrence, but with the value of the signal
-- at that time interval
capture :: SF NonInitialized (SVAppend (SVSignal a) (SVEvent b)) (SVEvent a)
capture = SF ((sampleEvt,) . captureInit . sampleValue . fst . splitSample)

captureInit :: a -> SF Initialized (SVAppend (SVSignal a) (SVEvent b)) (SVEvent a)
captureInit x = SFInit (flip (const . (deltaNothing, [],) . maybe (captureInit x) captureInit . deltaValue . fst . splitDelta))
                       ((, captureInit x) . const [occurrence x])

-- | SFEvalState
data SFEvalState m svIn svOut = SFEvalState { 
                                  esSF :: SF Initialized svIn svOut,
                                  esOutputHandlers :: SVHandler (m ()) svOut,
                                  esLastTime :: Double,
                                  esDelta :: SVDelta svIn
                                }

-- | A monad for evaluating signal functions
newtype SFEvalT svIn svOut m a = SFEvalT (StateT (SFEvalState m svIn svOut) m a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SFEvalT svIn svOut) where
  lift = SFEvalT . lift

-- | The most likely use case:
type SFEvalIO svIn svOut a = SFEvalT svIn svOut IO a

-- | Initialize the evaluation of a signal function, producing an SFEvalState.
initSFEval ::   SVHandler (m ()) svOut -- ^ The set of handlers for the output of the signal function
             -> SVSample svIn -- ^ The initial signal inputs for the signal function
             -> Double  -- ^ The initial time of the signal function
             -> SF NonInitialized svIn svOut -- ^ The signal function to evaluate
             -> SFEvalState m svIn svOut
initSFEval handlers initSample initTime (SF sigSampleF) = let (_, sf) = sigSampleF initSample in SFEvalState sf handlers initTime deltaNothing

-- | Evaluate a signal function, whose current state is stored in an SFEvalState.
runSFEvalT :: SFEvalT svIn svOut m a -> SFEvalState m svIn svOut -> m (a, SFEvalState m svIn svOut)
runSFEvalT  (SFEvalT m) sfES = runStateT m sfES

-- | Push an event occurrence into the input of the signal function
push :: (Monad m) => SVEventInput svIn -> SFEvalT svIn svOut m ()
push evtIn = SFEvalT $ do evalState <- get
                          let evtOcc = inputToOccurrence evtIn
                              (SFInit _ occCont) = esSF evalState
                              (occs, newSF) = occCont evtOcc
                          lift $ mapM_ (applyHandlerOccurrence $ esOutputHandlers evalState) occs
                          put (evalState { esSF = newSF })
                        
           

-- | Update the signal function input
update :: (Monad m) => SVSignalUpdate svIn -> SFEvalT svIn svOut m ()
update sigUpdate = SFEvalT $ do evalState <- get
                                let newDelta = updateDelta (esDelta evalState) sigUpdate
                                put (evalState { esDelta = newDelta })

-- | Sample the signal function
step ::   (Monad m) => Double -- ^ The latest time (not a time delta)
         -> SFEvalT svIn svOut m ()
step time = SFEvalT $ do evalState <- get
                         let dt = time - esLastTime evalState
                             (SFInit sampleF _) = esSF evalState
                             (deltaOut, occsOut, newSF) = sampleF dt (esDelta evalState)
                         lift $ sequence $ applyHandlerDelta (esOutputHandlers evalState) deltaOut
                         lift $ mapM_ (applyHandlerOccurrence $ esOutputHandlers evalState) occsOut
                         put (evalState { esLastTime = time, esDelta = deltaNothing, esSF = newSF })
