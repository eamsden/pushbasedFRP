{-# LANGUAGE KindSignatures, EmptyDataDecls, GADTs #-}
module SignalFunctions (
  -- * Datatypes
  SF(SF),
  NonInitialized,
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
  combineSignals
  
) where

import SignalVectors

-- | Signal function running or suspended
data Initialized :: *

-- | Signal function not yet running
data NonInitialized :: *

-- | Signal Function Type
data SF :: * -> * -> * -> * where
  SF     :: (SMemory Id svIn -> (SMemory Id svOut, SF Initialized svIn svOut)) 
            -> SF NonInitialized svIn svOut
  SFInit :: (Double -> ([SVIndex Id svOut], SF Initialized svIn svOut)) 
            -> (SVIndex Id svIn -> ([SVIndex Id svOut], SF Initialized svIn svOut)) 
            -> SF Initialized svIn svOut

-- | Identity signal function
identity :: SF NonInitialized sv sv
identity = SF (\mem -> (mem, identityInit))

identityInit :: SF Initialized sv sv
identityInit = SFInit (\dt -> ([], identityInit)) (\idx -> ([idx], identityInit))


-- | Lift a pure function to operate on signals
pureSignalTransformer :: (a -> b) -> SF NonInitialized (SVSignal a) (SVSignal b)
pureSignalTransformer f = SF (\mem -> (case mem of
                                         (SMSignal (Id x)) -> (SMSignal (Id $ f x))
                                         SMEmpty -> SMEmpty, pureSignalTransformerInit f))

pureSignalTransformerInit :: (a -> b) -> SF Initialized (SVSignal a) (SVSignal b)
pureSignalTransformerInit f = let psti = SFInit (\dt -> ([], psti)) (\(SVISignal (Id x)) -> ([SVISignal (Id $ f x)], psti))
                              in psti

-- | Lift a pure function to operate on events
pureEventTransformer :: (a -> b) -> SF NonInitialized (SVEvent a) (SVEvent b)
pureEventTransformer f = SF (\mem -> (case mem of
                                         (SMEvent (Id x)) -> (SMEvent (Id $ f x))
                                         SMEmpty -> SMEmpty, pureEventTransformerInit f))

pureEventTransformerInit :: (a -> b) -> SF Initialized (SVEvent a) (SVEvent b)
pureEventTransformerInit f = let pste = SFInit (\dt -> ([], pste)) (\(SVIEvent (Id x)) -> ([SVIEvent (Id $ f x)], pste))
                             in pste


-- | Sequential composition: the output of the left signal function is used as the input of the right signal function.
(>>>) :: SF NonInitialized svIn svBetween -> SF NonInitialized svBetween svOut -> SF NonInitialized svIn svOut
(SF memF1) >>> (SF memF2) = SF (\mem -> let (mem', sfInit1) = memF1 mem
                                            (mem'', sfInit2) = memF2 mem'
                                        in (mem'', composeInit sfInit1 sfInit2))

composeInit :: SF Initialized svIn svBetween -> SF Initialized svBetween svOut -> SF Initialized svIn svOut
composeInit (SFInit dtCont1 inputCont1) sf2@(SFInit dtCont2 inputCont2) =
  SFInit
    (\dt -> let (sf1Outputs, newSf1) = dtCont1 dt
                (sf2TimeOutputs, timeNewSf2) = dtCont2 dt
                (sf2FoldOutputs, newSf2) = foldl (\(outputs, SFInit _ inputCont) idx -> let (theseOutputs, nextSF) = inputCont idx in (theseOutputs ++ outputs, nextSF))
                                             ([], timeNewSf2)
                                             sf1Outputs
            in (sf2FoldOutputs ++ sf2TimeOutputs, composeInit newSf1 newSf2)
    )
    (\idx -> let (sf1Outputs, newSf1) = inputCont1 idx
                 (sf2FoldOutputs, newSf2) = foldl (\(outputs, SFInit _ inputCont) idx -> let (theseOutputs, nextSF) = inputCont idx in (theseOutputs ++ outputs, nextSF))
                                              ([], sf2)
                                              sf1Outputs
             in (sf2FoldOutputs, composeInit newSf1 newSf2)   
    )

-- | Apply a signal function to the first input, passing through the second
first :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend svIn sv) (SVAppend svOut sv)
first (SF memF) = SF (\mem -> case mem of
                                SMEmpty -> let (memOut, sf) = memF SMEmpty in (combineSignalMemory memOut SMEmpty, firstInit sf)
                                SMBoth x y -> let (memOut, sf) = memF x in (combineSignalMemory memOut y, firstInit sf))

firstInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend svIn sv) (SVAppend svOut sv)
firstInit (SFInit timeCont inputCont) = 
  let firstInitSF = SFInit (\dt -> let (changes, sf) = timeCont dt in (map SVILeft changes, firstInit sf))
                           (\change -> case change of
                                         SVILeft lChange -> let (changes, sf) = inputCont lChange in (map SVILeft changes, firstInit sf)
                                         SVIRight rChange -> ([SVIRight rChange], firstInitSF))
  in firstInitSF

-- | Apply a signal function to the second input, bypassing the first
second :: SF NonInitialized svIn svOut -> SF NonInitialized (SVAppend sv svIn) (SVAppend sv svOut)
second (SF memF) = SF (\mem -> case mem of
                                SMEmpty -> let (memOut, sf) = memF SMEmpty in (combineSignalMemory SMEmpty memOut, secondInit sf)
                                SMBoth x y -> let (memOut, sf) = memF y in (combineSignalMemory x memOut, secondInit sf))

secondInit :: SF Initialized svIn svOut -> SF Initialized (SVAppend sv svIn) (SVAppend sv svOut)
secondInit (SFInit timeCont inputCont) = 
  let secondInitSF = SFInit (\dt -> let (changes, sf) = timeCont dt in (map SVIRight changes, secondInit sf))
                            (\change -> case change of
                                          SVIRight rChange -> let (changes, sf) = inputCont rChange in (map SVIRight changes, secondInit sf)
                                          SVILeft lChange -> ([SVILeft lChange], secondInitSF))
  in secondInitSF

-- | Swap the inputs
swap :: SF NonInitialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swap = SF (\mem -> ((case mem of
                       SMEmpty -> SMEmpty
                       SMBoth x y -> SMBoth y x), swapInit))

swapInit :: SF Initialized (SVAppend sv1 sv2) (SVAppend sv2 sv1)
swapInit = SFInit (\dt -> ([], swapInit)) (\change -> ([case change of
                                                          SVILeft lChange -> SVIRight lChange
                                                          SVIRight rChange -> SVILeft rChange
                                                       ], swapInit))
    

-- | Copy the input
copy :: SF NonInitialized sv (SVAppend sv sv)
copy = SF (\mem -> (combineSignalMemory mem mem, copyInit))

copyInit :: SF Initialized sv (SVAppend sv sv)
copyInit = SFInit (\dt -> ([], copyInit)) (\change -> ([SVILeft change, SVIRight change], copyInit))

-- | Ignores its input
ignore :: SF NonInitialized sv SVEmpty
ignore = SF (\mem -> (SMEmpty, ignoreInit))

ignoreInit :: SF Initialized sv SVEmpty 
ignoreInit = SFInit (const ([], ignoreInit)) (const ([], ignoreInit))

-- | Remove the empty left side of the signal vector
cancelLeft :: SF NonInitialized (SVAppend SVEmpty sv) sv
cancelLeft = SF (\mem -> ((case mem of
                             SMEmpty -> SMEmpty
                             SMBoth x y -> y), cancelLeftInit))

cancelLeftInit :: SF Initialized (SVAppend SVEmpty sv) sv
cancelLeftInit = SFInit (const ([], cancelLeftInit)) (\change -> ([case change of
                                                                     SVIRight x -> x], cancelLeftInit))


-- | Add an empty left side to the signal vector
uncancelLeft :: SF NonInitialized sv (SVAppend SVEmpty sv)
uncancelLeft = SF (\mem -> (combineSignalMemory SMEmpty mem, uncancelLeftInit))

uncancelLeftInit :: SF Initialized sv (SVAppend SVEmpty sv)
uncancelLeftInit = SFInit (const ([], uncancelLeftInit)) (\change -> ([SVIRight change], uncancelLeftInit))

-- | Remove the empty right side of the signal vector
cancelRight :: SF NonInitialized (SVAppend sv SVEmpty) sv
cancelRight = SF (\mem -> ((case mem of
                              SMEmpty -> SMEmpty
                              SMBoth x y -> x), cancelRightInit))

cancelRightInit :: SF Initialized (SVAppend sv SVEmpty) sv
cancelRightInit = SFInit (const ([], cancelRightInit)) (\change -> ([case change of
                                                                       SVILeft x -> x], cancelRightInit))

-- | Add an empty right side to the signal vector
uncancelRight :: SF NonInitialized sv (SVAppend sv SVEmpty)
uncancelRight = SF (\mem -> (combineSignalMemory mem SMEmpty, uncancelRightInit))

uncancelRightInit :: SF Initialized sv (SVAppend sv SVEmpty)
uncancelRightInit = SFInit (const ([], uncancelRightInit)) (\change -> ([SVILeft change], uncancelRightInit))

-- | Change left to right association
associate :: SF NonInitialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associate = SF (\mem -> ((case mem of
                            SMEmpty -> SMEmpty
                            (SMBoth SMEmpty z) -> combineSignalMemory SMEmpty (combineSignalMemory SMEmpty z)
                            (SMBoth (SMBoth x y) z) -> combineSignalMemory x (combineSignalMemory y z)), associateInit))

associateInit :: SF Initialized (SVAppend (SVAppend sv1 sv2) sv3) (SVAppend sv1 (SVAppend sv2 sv3))
associateInit = SFInit (const ([], associateInit)) (\change -> ((case change of
                                                                   SVILeft (SVILeft x) -> [SVILeft x]
                                                                   SVILeft (SVIRight x) -> [SVIRight $ SVILeft x]
                                                                   SVIRight x -> [SVIRight $ SVIRight x]), associateInit))
         

-- | Change right to left association
unassociate :: SF NonInitialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociate = SF (\mem -> ((case mem of
                              SMEmpty -> SMEmpty
                              (SMBoth x SMEmpty) -> combineSignalMemory (combineSignalMemory x SMEmpty) SMEmpty
                              (SMBoth x (SMBoth y z)) -> combineSignalMemory (combineSignalMemory x y) z), unassociateInit))

unassociateInit :: SF Initialized (SVAppend sv1 (SVAppend sv2 sv3)) (SVAppend (SVAppend sv1 sv2) sv3)
unassociateInit = SFInit (const ([], unassociateInit)) (\change -> ((case change of
                                                                       SVILeft x             -> [SVILeft $ SVILeft x]
                                                                       SVIRight (SVILeft x)  -> [SVILeft $ SVIRight x]
                                                                       SVIRight (SVIRight x) -> [SVIRight x]), unassociateInit))

-- | Act as the given signal function until the first event, then act as the signal function carried in the event
switch :: SF NonInitialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF NonInitialized svIn svOut
switch (SF memF) = SF (\mem -> let (memOut, sfInit) = memF mem 
                               in ((case memOut of
                                      SMEmpty -> SMEmpty
                                      SMBoth x y -> x), switchInit mem sfInit)) 
                     
switchInit :: SMemory Id svIn -> SF Initialized svIn (SVAppend svOut (SVEvent (SF NonInitialized svIn svOut))) -> SF Initialized svIn svOut
switchInit mem sf@(SFInit timeCont inputCont) = 
  let switchInitSF = SFInit (\dt -> let (changes, newSF) = timeCont dt
                                        (outputChanges, switchEvents) = splitIndices changes
                                    in (outputChanges, case switchEvents of
                                                         (SVIEvent (Id (SF memF))):_ -> let (_, sfInit) = memF mem in sfInit
                                                         _ -> switchInitSF))
                            (\change -> let (changes, newSF) = inputCont change
                                            (outputChanges, switchEvents) = splitIndices changes
                                        in (outputChanges, case switchEvents of
                                                             (SVIEvent (Id (SF memF))):_ -> let (_, sfInit) = memF mem in sfInit
                                                             _ -> switchInitSF))
  in switchInitSF
                     

-- | Feed the right output back to the right input
loop :: SF NonInitialized (SVAppend svIn svLoop) (SVAppend svOut svLoop) -> SF NonInitialized svIn svOut
loop (SF memF) = SF (\mem -> let (memOutAndFeedback, sfInit) = memF memIn
                                 (memOut, memFeedback) = case memOutAndFeedback of
                                                           SMEmpty -> (SMEmpty, SMEmpty)
                                                           (SMBoth out feedback) -> (out, feedback)
                                 memIn = combineSignalMemory mem memFeedback
                             in (memOut, loopInit sfInit))

loopInit :: SF Initialized (SVAppend svIn svLoop) (SVAppend svOut svLoop) -> SF Initialized svIn svOut
loopInit (SFInit timeCont idxCont) = SFInit (\dt -> let (outputs, newSF) = timeCont dt
                                                        (newSF', outOutputs, feedbackOutputs) = foldl (\(sf@(SFInit _ inputCont), outs, feedbacks) idx -> 
                                                                                                        case idx of
                                                                                                          SVILeft out -> (sf, outs . (out:), feedbacks)
                                                                                                          SVIRight feedback -> let (outputs, newSF'') = 
                                                                                                                                    inputCont (SVIRight feedback)
                                                                                                                               in (newSF'', outs, feedbacks ++ outputs))
                                                                                                (newSF, id, [])
                                                                                                (outputs ++ feedbackOutputs)
                                                    in (outOutputs [], loopInit newSF'))
                                            (\idx -> let (outputs, newSF) = idxCont (SVILeft idx)
                                                         (newSF', outOutputs, feedbackOutputs) = foldl (\(sf@(SFInit _ inputCont), outs, feedbacks) idx -> 
                                                                                                        case idx of
                                                                                                          SVILeft out -> (sf, outs . (out:), feedbacks)
                                                                                                          SVIRight feedback -> let (outputs, newSF'') = 
                                                                                                                                    inputCont (SVIRight feedback)
                                                                                                                               in (newSF'', outs, feedbacks ++ outputs))
                                                                                                (newSF, id, [])
                                                                                                (outputs ++ feedbackOutputs)
                                                    in (outOutputs [], loopInit newSF')) 
                                                                                                  


-- | Produces the time as output
time :: SF NonInitialized SVEmpty (SVSignal Double)
time = SF (\_ -> (SMSignal (Id 0), timeInit 0))

timeInit :: Double -> SF Initialized SVEmpty (SVSignal Double)
timeInit t = SFInit (\dt -> ([SVISignal (Id (t + dt))], timeInit (t + dt))) (\_ -> ([], timeInit t))

-- | Delay events by a specified time. The time can be changed for all _following_ events by an event occurrence on the right input.
delay :: Double -> SF NonInitialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delay delayTime = SF (\_ -> (SMEmpty, delayInit [] delayTime 0))

delayInit :: [(Double, SVIndex Id (SVEvent a))] -> Double -> Double -> SF Initialized (SVAppend (SVEvent a) (SVEvent Double)) (SVEvent a)
delayInit eventsBuffer delayTime time = SFInit (\dt -> let newTime = time + dt
                                                           (readyEvents, notReadyEvents) = break ((>= newTime) . fst) eventsBuffer
                                                       in (map snd readyEvents, delayInit notReadyEvents delayTime newTime))
                                               (\change -> ([], case change of
                                                                  SVILeft evt -> delayInit (eventsBuffer ++ [(time + delayTime, evt)]) delayTime time
                                                                  SVIRight (SVIEvent (Id newDelayTime)) -> delayInit eventsBuffer newDelayTime time))
                                                            

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

integrate :: (TimeIntegrate i) => SF NonInitialized (SVSignal i) (SVSignal i)
integrate = SF (\mem -> case mem of
                          SMEmpty -> (SMEmpty, integrateNonInit)
                          SMSignal (Id x) -> (SMSignal (Id x), integrateInit iZero x x))

integrateNonInit :: (TimeIntegrate i) => SF Initialized (SVSignal i) (SVSignal i)
integrateNonInit = SFInit (const ([], integrateNonInit)) (\(SVISignal (Id x)) -> ([], integrateInit iZero x x))

integrateInit :: (TimeIntegrate i) => i -> i -> i -> SF Initialized (SVSignal i) (SVSignal i)
integrateInit currentSum lastStepValue currentValue = SFInit (\dt -> let newSum = currentSum `iPlus` ((currentValue `iDifference` lastStepValue) `iTimesDouble` dt)
                                                                     in ([SVISignal (Id newSum)], integrateInit newSum currentValue currentValue))
                                                             (\(SVISignal (Id x)) -> ([], integrateInit currentSum lastStepValue x))

union :: SF NonInitialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
union = SF (\_ -> (SMEmpty, unionInit))

unionInit :: SF Initialized (SVAppend (SVEvent a) (SVEvent a)) (SVEvent a)
unionInit = SFInit (\dt -> ([], unionInit)) (\change -> case change of
                                                          SVILeft lchange -> ([lchange], unionInit)
                                                          SVIRight rchange -> ([rchange], unionInit))

combineSignals :: ((a, b) -> c) -> SF NonInitialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignals f = SF (\mem -> ((case mem of
                                  (SMBoth (SMSignal (Id x)) (SMSignal (Id y))) -> SMSignal (Id (f (x, y)))
                                  _ -> SMEmpty), combineSignalsInit f mem))
combineSignalsInit :: ((a, b) -> c) -> SMemory Id (SVAppend (SVSignal a) (SVSignal b)) -> SF Initialized (SVAppend (SVSignal a) (SVSignal b)) (SVSignal c)
combineSignalsInit f mem = 
  let sf = SFInit (\dt -> ([], sf))
                  (\change -> let newMem = updateSMemory mem change
                              in case newMem of 
                                   SMBoth (SMSignal (Id x)) (SMSignal (Id y)) -> ([SVISignal (Id (f (x, y)))], combineSignalsInit f newMem)
                                   _ -> ([], combineSignalsInit f newMem))
  in sf

