{-# LANGUAGE KindSignatures, EmptyDataDecls, GADTs, ScopedTypeVariables, NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

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
  SF(),
  NonInitialized,
  Initialized,
  -- * Basic signal functions
  identity,
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
  -- * Evaluation
  SignalDelta,
  sd,
  sdLeft,
  sdRight,
  sdBoth,
  EventOccurrence,
  eo,
  eoLeft,
  eoRight,
  HandlerSet,
  sdHS,
  eoHS,
  hsLeft,
  hsRight,
  hsBoth,
  SFEvalT,
  SFEvalIO,
  SFEvalState,
  runSFEvalT,
  initSFEval,
  push,
  sample
  
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import FRP.TimeFlies.SignalVectors

-- | Signal function running or suspended
data Initialized :: *

-- | Signal function not yet running
data NonInitialized :: *

-- | Signal functions: The first type parameter distinguishes between
-- uninitialized (just specificied) and initialized (ready to respond to input
-- and time) signal functions. The second and third type parameters
-- are the input and output signal vectors, respectively. 
data SF :: * -> * -> * -> * where
  SF     :: (SMemory Id svIn -> (SMemory Id svOut, SF Initialized svIn svOut)) 
            -> SF NonInitialized svIn svOut
  SFInit :: (Double -> SMemory Id svIn -> (SMemory Id svOut, [SVIndex Id svOut], SF Initialized svIn svOut)) 
            -> (SVIndex Id svIn -> ([SVIndex Id svOut], SF Initialized svIn svOut)) 
            -> SF Initialized svIn svOut

-- Utility functions
-- | Apply a signal function to a list of changes, producing a list of
-- output changes and an updated signal function
applySF :: SF Initialized svIn svOut -> [SVIndex Id svIn] -> ([SVIndex Id svOut], SF Initialized svIn svOut)
applySF sf indices = foldr (\idx (changes, SFInit _ changeCont) -> let (newChanges, nextSF) = changeCont idx
                                                                   in (newChanges ++ changes, nextSF))
                     ([], sf)
                     indices

-- | Identity signal function: reproduce the input exactly as the output.
identity :: SF NonInitialized sv sv
identity = SF (\mem -> (mem, identityInit))

identityInit :: SF Initialized sv sv
identityInit = SFInit (\dt mem -> (mem, [], identityInit)) (\idx -> ([idx], identityInit))


-- | Apply the given function to every sample of a signal
pureSignalTransformer :: (a -> b) -> SF NonInitialized (SVSignal a) (SVSignal b)
pureSignalTransformer f = SF (\mem -> (case mem of
                                         (SMSignal (Id x)) -> (SMSignal (Id $ f x))
                                         SMEmpty -> SMEmpty, pureSignalTransformerInit f))

pureSignalTransformerInit :: (a -> b) -> SF Initialized (SVSignal a) (SVSignal b)
pureSignalTransformerInit f = let psti = SFInit (\dt mem -> (case mem of 
                                                               SMEmpty -> SMEmpty
                                                               SMSignal (Id x) -> SMSignal (Id $ f x),
                                                             [], psti))
                                                (\_ -> ([], psti))
                              in psti

-- | Apply the given function to each occurrence of an event
pureEventTransformer :: (a -> b) -> SF NonInitialized (SVEvent a) (SVEvent b)
pureEventTransformer f = SF (\mem -> (case mem of
                                         (SMEvent (Id x)) -> (SMEvent (Id $ f x))
                                         SMEmpty -> SMEmpty, pureEventTransformerInit f))

pureEventTransformerInit :: (a -> b) -> SF Initialized (SVEvent a) (SVEvent b)
pureEventTransformerInit f = let peti = SFInit (\dt mem -> (SMEmpty, [], peti)) (\(SVIEvent (Id x)) -> ([SVIEvent (Id $ f x)], peti))
                             in peti


-- | Produce a new signal function where the output of the first signal function is used as the input to the second
-- signal function
(>>>) :: SF NonInitialized svIn svBetween -> SF NonInitialized svBetween svOut -> SF NonInitialized svIn svOut
(SF memF1) >>> (SF memF2) = SF (\mem -> let (mem', sfInit1) = memF1 mem
                                            (mem'', sfInit2) = memF2 mem'
                                        in (mem'', composeInit sfInit1 sfInit2))

composeInit :: SF Initialized svIn svBetween -> SF Initialized svBetween svOut -> SF Initialized svIn svOut
composeInit (SFInit dtCont1 inputCont1) sf2@(SFInit dtCont2 inputCont2) =
  SFInit
    (\dt mem -> let (sf1MemOutput, sf1EvtOutputs, sf1New) = dtCont1 dt mem
                    (sf2MemOutput, sf2EvtOutputs, sf2New) = dtCont2 dt sf1MemOutput
                    (sf2EvtEvtOutputs, sf2Newest) = applySF sf2New sf1EvtOutputs
                in (sf2MemOutput, sf2EvtOutputs ++ sf2EvtEvtOutputs, composeInit sf1New sf2Newest)
    )
    (\idx -> let (sf1Outputs, newSf1) = inputCont1 idx
                 (sf2FoldOutputs, newSf2) = applySF sf2 sf1Outputs
             in (sf2FoldOutputs, composeInit newSf1 newSf2)   
    )

-- | Produce a new signal function where the left side of the input is used as
-- input for the given signal function, and the right side of the input
-- is combined unchanged with the output of the given signal function
first :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend svIn sv) (SVAppend svOut sv)
first (SF memF) = SF (\mem -> case mem of
                                SMEmpty -> let (memOut, sf) = memF SMEmpty in (combineSignalMemory memOut SMEmpty, firstInit sf)
                                SMBoth x y -> let (memOut, sf) = memF x in (combineSignalMemory memOut y, firstInit sf))

firstInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend svIn sv) (SVAppend svOut sv)
firstInit (SFInit timeCont inputCont) = 
  let firstInitSF = SFInit (\dt mem -> let (input, rightOutput) = case mem of
                                                                    SMEmpty -> (SMEmpty, SMEmpty)
                                                                    SMBoth sml smr -> (sml, smr)
                                           (memOutput, evtOutput, sf1New) = timeCont dt input
                                       in (combineSignalMemory memOutput rightOutput, map SVILeft evtOutput, firstInit sf1New))
                           (\change -> case change of
                                         SVILeft lChange -> let (changes, sf) = inputCont lChange in (map SVILeft changes, firstInit sf)
                                         SVIRight rChange -> ([SVIRight rChange], firstInitSF))
  in firstInitSF

-- | Similar to 'first', except the right side of the signal vector is used
-- as input for the given signal function, and the left side is combined
-- unchanged with the output.
second :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend sv svIn) (SVAppend sv svOut)
second (SF memF) = SF (\mem -> case mem of
                                SMEmpty -> let (memOut, sf) = memF SMEmpty in (combineSignalMemory SMEmpty memOut, secondInit sf)
                                SMBoth x y -> let (memOut, sf) = memF y in (combineSignalMemory x memOut, secondInit sf))

secondInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend sv svIn) (SVAppend sv svOut)
secondInit (SFInit timeCont inputCont) = 
  let secondInitSF = SFInit (\dt mem -> let (leftOutput, input) = case mem of
                                                                     SMEmpty -> (SMEmpty, SMEmpty)
                                                                     SMBoth sml smr -> (sml, smr)
                                            (memOutput, evtOutput, sf1New) = timeCont dt input
                                        in (combineSignalMemory leftOutput memOutput , map SVIRight evtOutput, secondInit sf1New))
                            (\change -> case change of
                                          SVIRight rChange -> let (changes, sf) = inputCont rChange in (map SVIRight changes, secondInit sf)
                                          SVILeft lChange -> ([SVILeft lChange], secondInitSF))
  in secondInitSF

-- | Produce the left side of the input as the right side of the output, 
-- and vice vers&#226;.
swap :: SF NonInitialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swap = SF (\mem -> ((case mem of
                       SMEmpty -> SMEmpty
                       SMBoth x y -> SMBoth y x), swapInit))

swapInit :: SF Initialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swapInit = SFInit (\dt mem -> (case mem of
                                 SMEmpty -> SMEmpty
                                 SMBoth sml smr -> SMBoth smr sml,
                               [], swapInit)) 
                  (\change -> ([case change of
                                  SVILeft lChange -> SVIRight lChange
                                  SVIRight rChange -> SVILeft rChange
                               ], swapInit))
    

-- | Produce the input as left and right sides of the output
copy :: SF NonInitialized sv (SVAppend sv sv)
copy = SF (\mem -> (combineSignalMemory mem mem, copyInit))

copyInit :: SF Initialized sv (SVAppend sv sv)
copyInit = SFInit (\_ mem -> (combineSignalMemory mem mem, [], copyInit)) 
                  (\change -> ([SVILeft change, SVIRight change], copyInit))

-- | Discard the input entirely and produce the left side of the output
ignore :: SF NonInitialized sv SVEmpty
ignore = SF (\mem -> (SMEmpty, ignoreInit))

ignoreInit :: SF Initialized sv SVEmpty 
ignoreInit = SFInit (const $ const (SMEmpty, [], ignoreInit)) (const ([], ignoreInit))

-- | Discard the empty left side of the input and use the right side as
-- output.
cancelLeft :: SF NonInitialized (SVAppend SVEmpty sv) sv
cancelLeft = SF (\mem -> ((case mem of
                             SMEmpty -> SMEmpty
                             SMBoth _ y -> y), cancelLeftInit))

cancelLeftInit :: SF Initialized (SVAppend SVEmpty sv) sv
cancelLeftInit = SFInit (\_ mem -> (case mem of
                                      SMEmpty -> SMEmpty
                                      SMBoth sml smr -> smr, [], cancelLeftInit))
                        (\change -> ([case change of
                                        SVIRight x -> x], cancelLeftInit))


-- | Use the input as the right side of the output and leave the left side empty
uncancelLeft :: SF NonInitialized sv (SVAppend SVEmpty sv)
uncancelLeft = SF (\mem -> (combineSignalMemory SMEmpty mem, uncancelLeftInit))

uncancelLeftInit :: SF Initialized sv (SVAppend SVEmpty sv)
uncancelLeftInit = SFInit (\_ mem -> (combineSignalMemory SMEmpty mem, [], uncancelLeftInit)) 
                          (\change -> ([SVIRight change], uncancelLeftInit))

-- | Discard the empty right side of the input and use the left side as the
-- output.
cancelRight :: SF NonInitialized (SVAppend sv SVEmpty) sv
cancelRight = SF (\mem -> ((case mem of
                              SMEmpty -> SMEmpty
                              SMBoth x y -> x), cancelRightInit))

cancelRightInit :: SF Initialized (SVAppend sv SVEmpty) sv
cancelRightInit = SFInit (\_ mem -> (case mem of
                                       SMEmpty -> SMEmpty
                                       SMBoth sml smr -> sml, [], cancelRightInit))
                         (\change -> ([case change of
                                         SVILeft x -> x], cancelRightInit))

-- | Use the input as the left side of the output and leave the right side empty
uncancelRight :: SF NonInitialized sv (SVAppend sv SVEmpty)
uncancelRight = SF (\mem -> (combineSignalMemory mem SMEmpty, uncancelRightInit))

uncancelRightInit :: SF Initialized sv (SVAppend sv SVEmpty)
uncancelRightInit = SFInit (\_ mem -> (combineSignalMemory mem SMEmpty, [], uncancelRightInit))
                           (\change -> ([SVILeft change], uncancelRightInit))

-- | Take an input where the left two subvectors are associated and 
-- produce an output with the same subvectors, but where the right
-- two subvectors are associated.
associate :: SF NonInitialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associate = SF (\mem -> ((case mem of
                            SMEmpty -> SMEmpty
                            (SMBoth SMEmpty z) -> combineSignalMemory SMEmpty (combineSignalMemory SMEmpty z)
                            (SMBoth (SMBoth x y) z) -> combineSignalMemory x (combineSignalMemory y z)), associateInit))

associateInit :: SF Initialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associateInit = SFInit (\_ mem -> (case mem of
                                     SMEmpty -> SMEmpty
                                     SMBoth sml smr -> case sml of
                                                         SMEmpty -> combineSignalMemory SMEmpty (combineSignalMemory SMEmpty smr)
                                                         SMBoth smll smlr -> combineSignalMemory smll (combineSignalMemory smlr smr),
                                   [], associateInit)) 
                       (\change -> ((case change of
                                       SVILeft (SVILeft x) -> [SVILeft x]
                                       SVILeft (SVIRight x) -> [SVIRight $ SVILeft x]
                                       SVIRight x -> [SVIRight $ SVIRight x]), associateInit))
         

-- | Take an input where the right two subvectors are associated and produce an
-- output with the same subvectors, but where the left two subvectors are
-- associated.
unassociate :: SF NonInitialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociate = SF (\mem -> ((case mem of
                              SMEmpty -> SMEmpty
                              (SMBoth x SMEmpty) -> combineSignalMemory (combineSignalMemory x SMEmpty) SMEmpty
                              (SMBoth x (SMBoth y z)) -> combineSignalMemory (combineSignalMemory x y) z), unassociateInit))

unassociateInit :: SF Initialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociateInit = SFInit (\_ mem -> (case mem of
                                       SMEmpty -> SMEmpty
                                       SMBoth sml smr -> case smr of
                                                           SMEmpty -> combineSignalMemory (combineSignalMemory sml SMEmpty) SMEmpty
                                                           SMBoth smrl smrr -> combineSignalMemory (combineSignalMemory sml smrl) smrr,
                                     [], unassociateInit)) 
                         (\change -> ((case change of
                                         SVILeft x             -> [SVILeft $ SVILeft x]
                                         SVIRight (SVILeft x)  -> [SVILeft $ SVIRight x]
                                         SVIRight (SVIRight x) -> [SVIRight x]), unassociateInit))


-- | Provide the input as input to the given signal function, producing
-- the left side of the given signal function's output as output. Upon
-- the first occurrence of the event on the right side of the given signal
-- function's output, act as the signal function contained by the occurrence.
-- Note that the if a switch event occurs in between time samples,
-- the switch function will combine the signal deltas produced by the initialization
-- of the new signal function and by the first time step of the new signal function.
switch :: SF NonInitialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF NonInitialized svIn svOut
switch (SF memF) = SF (\mem -> let (sfMemOut, sf) = memF mem
                                   memOut = case sfMemOut of
                                              SMEmpty -> SMEmpty
                                              SMBoth sml _ -> sml
                               in (memOut, switchInit mem sf))

switchInit :: SMemory Id svIn -> SF Initialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF Initialized svIn svOut
switchInit inputMem sf@(SFInit timeCont changeCont) =
  SFInit (\dt mem -> let (memOut, changesOut, newSF) = timeCont dt mem
                         newInputMem = updateWithSMemory mem inputMem
                         (outputChanges, switchChanges) = splitIndices changesOut
                         (outputMem, nextSF) = case switchChanges of
                                                 SVIEvent (Id (SF memF)):_ -> memF newInputMem
                                                 [] -> (case memOut of
                                                          SMEmpty -> SMEmpty
                                                          SMBoth sml _ -> sml,
                                                        switchInit newInputMem newSF)
                     in (outputMem, outputChanges, nextSF))
         (\change -> let (changesOut, newSF) = changeCont change
                         (outputChanges, switchChanges) = splitIndices changesOut
                         nextSF = case switchChanges of
                                    (SVIEvent (Id (SF memF))):_ -> let (memOut, newSF') = memF inputMem
                                                                   in switchWait memOut newSF'
                                    [] -> switchInit inputMem newSF
                     in (outputChanges, nextSF))

switchWait :: SMemory Id svOut -> SF Initialized svIn svOut -> SF Initialized svIn svOut
switchWait outputMem sf@(SFInit timeCont changeCont) = SFInit (\dt mem -> let (memOutDelta, outputChanges, newSF) = timeCont dt mem
                                                                          in (updateWithSMemory memOutDelta outputMem, outputChanges, newSF))
                                                              (\change -> let (outputChanges, newSF) = changeCont change
                                                                          in (outputChanges, switchWait outputMem newSF))
                                    
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
loop (SF memF) = SF (\mem -> let (memOut, sfInit) = memF (combineSignalMemory mem SMEmpty)
                                 output = 
                                   case memOut of
                                     SMEmpty -> SMEmpty
                                     SMBoth out _ -> out
                              in (output, loopInit SMEmpty sfInit))

loopInit :: SMemory Id svLoop -> SF Initialized (SVAppend svIn svLoop) (SVAppend svOut svLoop) -> SF Initialized svIn svOut
loopInit loopMem (SFInit timeCont changeCont) = SFInit (\dt mem -> let (memOut, changesOut, newSF) = timeCont dt (combineSignalMemory mem loopMem)
                                                                       (changesOutput, loopChanges) = splitIndices changesOut
                                                                       (changesOutput', loopChanges') = splitIndices foldChanges
                                                                       (foldChanges, newSF') = applySF newSF (map SVIRight (loopChanges ++ loopChanges'))
                                                                       (outputMem, newLoopMem) = case memOut of
                                                                                                   SMEmpty -> (SMEmpty, SMEmpty)
                                                                                                   SMBoth sml smr -> (sml, smr)
                                                                   in (outputMem, changesOutput ++ changesOutput', loopInit newLoopMem newSF'))
                                                       (\change -> let (changesOut, newSF) = changeCont $ SVILeft change
                                                                       (changesOutput, loopChanges) = splitIndices changesOut
                                                                       (changesOutput', loopChanges') = splitIndices foldChanges
                                                                       (foldChanges, newSF') = applySF newSF (map SVIRight (loopChanges ++ loopChanges'))
                                                                   in (changesOutput ++ changesOutput', loopInit loopMem newSF'))
                                                                       
                                          


-- | With an empty input, produce the time since the signal function began
-- running as a signal output.
time :: SF NonInitialized SVEmpty (SVSignal Double)
time = SF (\_ -> (SMSignal (Id 0), timeInit 0))

timeInit :: Double -> SF Initialized SVEmpty (SVSignal Double)
timeInit t = SFInit (\dt _ -> (SMSignal (Id (t + dt)), [], timeInit (t + dt)))
                    (\_ -> ([], timeInit t))

-- | Maintain a time delay for events. The initial argument is the initial time
-- delay. Left input event occurrences are stored as state together with the 
-- time delay at occurrence time. After that delay has expired, a corresponding 
-- event will be produced on the output. Right input event occurrences update
-- the delay time. Note that this means that later input events may be output
-- before prior input events if the delay time is shortened in the intervening
-- time.

delay :: Double -> SF NonInitialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delay delayTime = SF (\_ -> (SMEmpty, delayInit [] delayTime 0))

delayInit :: [(Double, SVIndex Id (SVEvent a))] -> Double -> Double -> SF Initialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delayInit eventsBuffer delayTime time = SFInit (\dt _ -> let newTime = time + dt
                                                             (readyEvents, notReadyEvents) = break ((>= newTime) . fst) eventsBuffer
                                                         in (SMEmpty, map snd readyEvents, delayInit notReadyEvents delayTime newTime))
                                               (\change -> ([], case change of
                                                                  SVILeft evt -> delayInit (eventsBuffer ++ [(time + delayTime, evt)]) delayTime time
                                                                  SVIRight (SVIEvent (Id newDelayTime)) -> delayInit eventsBuffer newDelayTime time))
                                                            

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
integrate = SF (\mem -> case mem of
                          SMEmpty -> (SMEmpty, integrateNonInit)
                          SMSignal (Id x) -> (SMSignal (Id x), integrateInit iZero x))

integrateNonInit :: (TimeIntegrate i) => SF Initialized (SVSignal i) (SVSignal i)
integrateNonInit = SFInit (\dt mem -> case mem of 
                                        SMEmpty -> (SMEmpty, [], integrateNonInit)
                                        SMSignal (Id x) -> (SMSignal (Id iZero), [], integrateInit iZero x)) 
                          (\_ -> ([], integrateNonInit))

integrateInit :: (TimeIntegrate i) => i -> i -> SF Initialized (SVSignal i) (SVSignal i)
integrateInit currentSum currentValue = SFInit (\dt mem -> let (newSum, newVal) = case mem of
                                                                                    SMEmpty -> ((currentValue `iTimesDouble` dt) `iPlus` currentSum, currentValue)
                                                                                    SMSignal (Id x)-> ((x `iTimesDouble` dt) `iPlus` currentSum, x)
                                                           in (SMSignal (Id newSum), [], integrateInit newSum newVal))
                                               (\_ -> ([], integrateInit currentSum currentValue))

-- | Produce an event occurrence on the output corresponding to an event
-- occurrence on either input
union :: SF NonInitialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
union = SF (\_ -> (SMEmpty, unionInit))

unionInit :: SF Initialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
unionInit = SFInit (\dt _ -> (SMEmpty, [], unionInit)) (\change -> case change of
                                                                     SVILeft lchange -> ([lchange], unionInit)
                                                                     SVIRight rchange -> ([rchange], unionInit))

-- | Combine both inputs at each point in time using the given function.
combineSignals :: ((a, b) -> c) -> SF NonInitialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignals f = SF (\mem -> ((case mem of
                                  (SMBoth (SMSignal (Id x)) (SMSignal (Id y))) -> SMSignal (Id (f (x, y)))
                                  _ -> SMEmpty), combineSignalsInit f mem))
combineSignalsInit :: ((a, b) -> c) -> SMemory Id (SVAppend (SVSignal a) (SVSignal b)) -> SF Initialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignalsInit f currentMem = 
  let sf = SFInit (\dt mem -> let newMem = updateWithSMemory mem currentMem
                              in case newMem of
                                   SMBoth (SMSignal (Id x)) (SMSignal (Id y)) -> (SMSignal (Id (f (x, y))), [], combineSignalsInit f newMem)
                                   _ -> (SMEmpty, [], combineSignalsInit f newMem))
                  (\_ -> ([], sf))
  in sf

-- | SFEvalState
data SFEvalState m svIn svOut = SFEvalState { 
                                esSF :: SF Initialized svIn svOut,
                                esOutputHandlers :: SMemory (To (m ())) svOut
                              }

-- | A monad for evaluating signal functions
newtype SFEvalT svIn svOut m a = SFEvalT (StateT (SFEvalState m svIn svOut) m a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SFEvalT svIn svOut) where
  lift = SFEvalT . lift

-- | The most likely use case:
type SFEvalIO svIn svOut a = SFEvalT svIn svOut IO a

-- | Descriptors of signal changes for a time step
newtype SignalDelta sv = SignalDelta (SMemory Id sv)

-- | Create a signal delta for a singleton input
sd :: a -> SignalDelta (SVSignal a)
sd = SignalDelta . SMSignal . Id

-- | Use this signal delta for only the left input
sdLeft :: SignalDelta svl -> SignalDelta (SVAppend svl svr)
sdLeft (SignalDelta sml) = SignalDelta $ combineSignalMemory sml SMEmpty 

-- | Use this signal delta for only the right input
sdRight :: SignalDelta svr -> SignalDelta (SVAppend svl svr)
sdRight (SignalDelta smr) = SignalDelta $ combineSignalMemory SMEmpty smr

-- | Use these signal deltas for the left and right inputs, respectively
sdBoth :: SignalDelta svl -> SignalDelta svr -> SignalDelta (SVAppend svl svr)
sdBoth (SignalDelta sml) (SignalDelta smr) = SignalDelta $ combineSignalMemory sml smr

-- | Descriptors of event occurrences
newtype EventOccurrence sv = EventOccurrence (SVIndex Id sv)

-- | Create an event occurrence to push
eo :: a -> EventOccurrence (SVEvent a)
eo = EventOccurrence . SVIEvent . Id

-- | Route an event occurrence left
eoLeft :: EventOccurrence svl -> EventOccurrence (SVAppend svl svr)
eoLeft (EventOccurrence svi) = EventOccurrence (SVILeft svi)

-- | Route an event occurrence right
eoRight :: EventOccurrence svr -> EventOccurrence (SVAppend svl svr)
eoRight (EventOccurrence svr) = EventOccurrence (SVIRight svr)

-- | A set of actions to handle SF outputs. These outputs may be
-- the components of a signal delta or may be event occurrences.
newtype HandlerSet sv m = HandlerSet (SMemory (To (m ())) sv)

-- | A handler for a signal delta output
sdHS :: (a -> m ()) -> HandlerSet (SVSignal a) m
sdHS = HandlerSet . SMSignal . To

-- | A handler for an event occurrence output
eoHS :: (a -> m()) -> HandlerSet (SVEvent a) m
eoHS = HandlerSet . SMEvent . To

-- | Use the given handler for the left side of the output, and do not handle
-- the right.
hsLeft :: HandlerSet svl m -> HandlerSet (SVAppend svl svr) m
hsLeft (HandlerSet sm) = HandlerSet $ combineSignalMemory sm SMEmpty

-- | Use the given handler for the right side of the output, and do not handle
-- left side.
hsRight :: HandlerSet svr m -> HandlerSet (SVAppend svl svr) m
hsRight (HandlerSet sm) = HandlerSet $ combineSignalMemory SMEmpty sm

-- | Use the given handlers for the left and right sides of the output,
-- respectively.
hsBoth :: HandlerSet svl m -> HandlerSet svr m -> HandlerSet (SVAppend svl svr) m
hsBoth (HandlerSet sml) (HandlerSet smr) = HandlerSet $ combineSignalMemory sml smr

-- | Initialize the evaluation of a signal function, producing an SFEvalState.
initSFEval :: HandlerSet svOut m -> SF NonInitialized svIn svOut -> SFEvalState m svIn svOut
initSFEval (HandlerSet handlerMem) (SF memF) = let (_, sf) = memF SMEmpty in SFEvalState sf handlerMem

-- | Evaluate a signal function, whose current state is stored in an SFEvalState.
runSFEvalT :: SFEvalT svIn svOut m a -> SFEvalState m svIn svOut -> m (a, SFEvalState m svIn svOut)
runSFEvalT  (SFEvalT m) sfES = runStateT m sfES

-- | Push an event occurrence into the input of the signal function
push :: (Monad m) => EventOccurrence svIn -> SFEvalT svIn svOut m ()
push (EventOccurrence svi) = SFEvalT $ do
  (SFEvalState (SFInit _ changeCont) handlerMem) <- get
  let (changes, newSF) = changeCont svi
  lift $ sequence $ applySMTo handlerMem changes
  put (SFEvalState newSF handlerMem)

-- | Sample the signal function
sample :: (Monad m) => Double -> SignalDelta svIn -> SFEvalT svIn svOut m ()
sample dt (SignalDelta deltaIn) = SFEvalT $ do
  (SFEvalState (SFInit timeCont _) handlerMem) <- get
  let (deltaOut, changes, newSF) = timeCont dt deltaIn
  lift $ sequence $ applySMToSM handlerMem deltaOut
  lift $ sequence $ applySMTo handlerMem changes
  put (SFEvalState newSF handlerMem)
