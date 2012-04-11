{-# LANGUAGE EmptyDataDecls, GADTs #-}
module ReactiveFunctions (
  -- * Reactive function type
  ReactiveFunction, Initialized, NonInitialized
  -- * Reactive function accessors (for use in eval implementation)
  occurrenceContinuation, changeContinuation, timeContinuation, initFunction,
  -- * Reactive function constructors
  -- ** Routing functions
  rfIdentity,
  rfCompose,
  rfFirst,     rfSecond,
  rfCancelL,   rfCancelR,
  rfUncancelL, rfUncancelR,
  rfAssoc,     rfUnassoc
  rfCopy,
  rfDrop,
  rfSwap,
  rfLoop,
  -- ** Signal functions
  rfSigPure,
  rfIntegral,
  rfCombine,
  rfHoldEvent,
  -- ** Event functions
  rfEventPure,
  rfAfter,
  rfCapture,
  rfPredicate,
  rfChange,
  rfEventAccum,
  -- ** Reactivity
  rfSwitch

) where

import ReactiveVectors

type DTime = Double

data Initialized
data NonInitialized

data ReactiveFunction * -> * -> * -> * where
  -- | Combinators and event functions for switches produce
  --   non-initialized reactive functions. A non-initialized
  --   reactive function may not depend on a signal being
  --   remembered for any input in order to function properly.
  ReactiveFunctionNonInitialized ::   (SignalMemory v1 -> (ReactiveFunction Initialized v1 v2, SignalMemory v2)) 
                                      -> ReactiveFunction NonInitialized v1 v2
  -- | Time dependent functions may produce new events or signals as a result of time advancing
  --   Internally, signal changes and events should be propagated ahead of time advances.
  ReactiveFunction  ::   (DTime -> ([Change v2], ReactiveFunction Initialized v2)) 
                      -> (Change v1 -> ([Change v2], ReactiveFunction Initialized v2))
                      -> ReactiveFunction Initialized v1 v2


-- | Selector for change continuations
changeContinuation ::   ReactiveFunction Initialized v1 v2 
                     -> Change v1 
                     -> ([Change v2], ReactiveFunction Initialized v2)
changeContinuation (ReactiveFunction _ cont) = cont

-- | Selector for time step continuations (returns a do-nothing continuation for time-independent reactive functions)
timeContinuation ::   ReactiveFunction Initialized v1 v2 
                   -> DTime 
                   -> ([Change v2], ReactiveFunction Initialized v2)
timeContinuation (ReactiveFunctionTimeDependent cont _) = cont

initFunction ::   ReactiveFunction NonInitialized v1 v2
               -> (SignalMemory v1 -> (ReactiveFunction Initialized v1 v2, SignalMemory v2)) 
               -> ReactiveFunction NonInitialized v1 v2
initFunction (ReactiveFunctionNonInitialized f) = f


-- | The identity reactive function. This function simply propagates 
--   signals and events on all inputs to the corresponding outputs.
rfIdentity :: ReactiveFunction NonInitialized v1 v1
rfIdentity = ReactiveFunctionNonInitialized (\memory -> 
                                                 let rf = ReactiveFunction
                                                            (\dt -> ([], rf)) 
                                                            (\change -> ([change], rf))
                                                 in (rf, memory)
                                            )

rfCompose ::   ReactiveFunction NonInitialized v1 v2 
            -> ReactiveFunction NonInitialized v2 v3 
            -> ReactiveFunction NonInitialized v1 v3
rfCompose (ReactiveFunctionNonInitialized initF1) (ReactiveFunctionNonInitialized initF2) =
  ReactiveFunctionNonInitialized (\memory -> let (rfInit1, memory')  = initF1 memory
                                                 (rfInit2, memory'') = initF2 memory'
                                             in (rfComposeInit rfInit1 rfInit2, memory''))

rfComposeInit ::   ReactiveFunction Initialized v1 v2
                -> ReactiveFunction Initialized v2 v3
                -> ReactiveFunction Initialized v1 v3
rfComposeInit (ReactiveFunction timeCont1 changeCont1) rf2@(ReactiveFunction timeCont2 _) =
  ReactiveFunction 
    (\dt -> let (changes1, rf1') = timeCont1 dt
                (changes2, rf2') = timeCont2 dt
                (changes2', rf2Out) = foldl (\(changesAcc, (ReactiveFunction _ changeCont2')) change -> let (changesOutput, rf2'') = changeCont2' change
                                                                                                        in (changesAcc ++ changesOutput, rf2''))
                                      ([], rf2')
                                      changes1
            in (changes2 ++ changes2', rf2Out))
    (\change -> let (changes1, rf1') = changeCont1 change
                in foldl (\(changesAcc, (ReactiveFunction _ changeCont2')) change -> let (changesOutput, rf2'') = changeCont2' change
                                                                                                         in (changesAcc ++ changesOutput, rf2''))
                   ([], rf2)
                   changes1)
              
                   
                

rfFirst ::   ReactiveFunction NonInitialized v1 v2 
          -> ReactiveFunction NonInitialized (RVAppend v1 v3) (RVAppend v2 v3)
rfFirst (ReactiveFunctionNonInitialized initF) = 
  ReactiveFunctionNonIntialized (\memory -> let (initRF, memoryLeft) = initF $ signalMemoryLeft memory
                                            in (rfFirstInit initRF, signalMemoryCombine memoryLeft (signalMemoryRight memory)))

rfFirstInit ::   ReactiveFunction Initialized v1 v2
              -> ReactiveFunction Initialized (RVAppend v1 v3) (RVAppend v2 v3)
rfFirstInit (ReactiveFunction rfTimeCont rfChangeCont rfOccurrenceCont) =
  let rf = ReactiveFunction
             (\dtime -> let (changes, occurrences, rf') = rfTimeCont dtime
                        in (map ChangeLeft changes, map OLeft occurrences, rfFirstInit rf')
             )
             (\change -> case change of
                                 ChangeLeft change' -> let (changes, rf') = rfChangeCont change'
                                                       in (map ChangeLeft changes, rfFirstInit rf')
                                 changeRight@(ChangeRight _) -> ([changeRight], rf)
             )


rfSecond ::   ReactiveFunction NonInitialized v1 v2 
           -> ReactiveFunction NonInitialized (RVAppend v3 v1) (RVAppend v3 v1)
rfSecond (ReactiveFunctionNonInitialized initF) = 
  ReactiveFunctionNonIntialized (\memory -> let (initRF, memoryRight) = initF $ signalMemoryRight memory
                                            in (rfFirstInit initRF, signalMemoryCombine (signalMemoryLeft memory) memoryRight))

rfSecondInit ::   ReactiveFunction Initialized v1 v2
               -> ReactiveFunction Initialized (RVAppend v3 v1) (RVAppend v3 v1)
rfSecondInit (ReactiveFunction rfTimeCont rfChangeCont rfOccurrenceCont) =
  let rf = ReactiveFunction
             (\dtime -> let (changes, occurrences, rf') = rfTimeCont dtime
                        in (map ChangeRight changes, map ORight occurrences, rfSecondInit rf')
             )
             (\change -> case change of
                                 ChangeRight change' -> let (changes, occurrences, rf') = rfChangeCont change'
                                                        in (map ChangeRight changes, rfSecondInit rf')
                                 changeLeft@(ChangeLeft _) -> ([changeLeft], rf)
             )

rfCancelL :: ReactiveFunction NonInitialized (RVAppend RVEmpty v1) v1
rfCancelL = 
  ReactiveFunctionNonInitialized 
    (\memory -> (signalMemoryRight memory,
                 let rf = ReactiveFunction 
                            (\_ -> ([], rf))
                            (\(ChangeRight change') -> ([change'], rf)) 
                 in rf))

rfCancelR :: ReactiveFunction NonInitialized (RVAppend v1 RVEmpty) v1
rfCancelR =
  ReactiveFunctionNonInitialized
    (\memory -> (signalMemoryLeft memory,
                 let rf = ReactiveFunction
                            (\_ -> ([], rf))
                            (\(ChangeLeft change') -> ([change'], rf))
                 in rf))

rfUncancelL :: ReactiveFunction NonInitialized v1 (RVAppend RVEmpty v1)
rfUncancelL = ReactiveFunctionNonInitialized
  (\memory -> (signalMemoryCombine SignalMemoryUnknown memory,  
               let rf = ReactiveFunction
                          (\_ -> ([], rf))
                          (\change -> ([ChangeRight change], rf))
               in rf))

rfUncancelR :: ReactiveFunction NonInitialized v1 (RVAppend v1 RVEmpty)
rfUncancelR = ReactiveFunctionNonInitialized
  (\memory -> (signalMemoryCombine SignalMemoryUnknown memory,
               let rf = ReactiveFunction
                          (\_ -> ([], rf))
                          (\change -> ([ChangeLeft change], rf))
               in rf))

rfAssoc :: ReactiveFunction NonInitialized (RVAppend (RVAppend v1 v2) v3) (RVAppend v1 (RVAppend v2 v3))
rfAssoc = ReactiveFunctionNonInitialized
  (\memory -> (signalMemoryCombine (signalMemoryLeft $ SignalMemoryLeft memory) 
                                   (signalMemoryCombine (signalMemoryRight $ signalMemoryLeft memory) (signalMemoryRight memory)),
               let rf = ReactiveFunction
                          (\_ -> ([], [], rf))
                          (\change -> ([case change of
                                          (ChangeLeft (ChangeLeft change')) -> ChangeLeft change'
                                          (ChangeLeft (ChangeRight change')) -> ChangeRight $ ChangeLeft change'
                                          (ChangeRight change') -> ChangeRight $ ChangeRight change'], rf))
               in rf))
                                         
                  
                         

rfUnassoc :: ReactiveFunction NonInitialized (RVAppend v1 (RVAppend v2 v3)) (RVAppend (RVAppend v1 v2) v3)
rfUnassoc = ReactiveFunctionNonInitialized 
  (\memory -> (signalMemoryCombine (signalMemoryCombine (signalMemoryLeft memory) (signalMemoryLeft $ signalMemoryRight memory))
               (signalMemoryRight $ signalMemoryRight memory),
               let rf = ReactiveFunction
                          (\_ -> ([], [], rf))
                          (\change -> ([case change of
                                          (ChangeLeft change') -> ChangeLeft $ ChangeLeft change'
                                          (ChangeRight (ChangeLeft change')) -> ChangeLeft $ ChangeRight change'
                                          (ChangeRight (ChangeRight change')) -> ChangeRight change'], rf))
               in rf))

rfCopy :: ReactiveFunction NonInitialized v1 (RVAppend v1 v1)
rfCopy = ReactiveFunctionNonInitialized 
  (\memory -> (signalMemoryCombine memory memory,
               let rf = ReactiveFunction
                          (\_ -> ([], [], rf))
                          (\change -> ([ChangeLeft change, ChangeRight change], rf))
               in rf))

rfDrop :: ReactiveFunction NonInitialized v1 RVEmpty
rfDrop = ReactiveFunctionNonInitialized
  (\_ -> (SignalMemoryUnknown,
          let rf = ReactiveFunction
                     (\_ -> ([], rf))
                     (\_ -> ([], rf))
                     (\_ -> ([], rf))
          in rf))

rfSwap :: ReactiveFunction NonInitialized (RVAppend v1 v2) (RVAppend v2 v1)
rfSwap = ReactiveFunctionNonInitialized
  (\memory -> (signalMemoryCombine (signalMemoryRight memory) (signalMemoryLeft memory),
               let rf = ReactiveFunction
                          (\_ -> ([], [], rf))
                          (\change -> ([case change of
                                          ChangeLeft change' -> ChangeRight change'
                                          ChangeRight change' -> ChangeLeft change'
                                       ], rf))
               in rf))

rfLoop :: ReactiveFunction NonInitialized (RVAppend v1 v2) (RVAppend v3 v4) -> ReactiveFunction NonInitialized v1 v3
rfLoop (ReactiveFunctionNonInitialized initF) = ReactiveFunctionNonInitialized (\memory -> let (internalMemoryOut, internalRF) = 
                                                                                                 initF $ signalFunctionCombine memory
                                                                                                                               (signalMemoryRight internalMemoryOut)
                                                                                           in (signalMemoryLeft internalMemoryOut, rfLoopInit internalRF))

rfLoopInit :: ReactiveFunction NonInitialized (RVAppend v1 v2) (RVAppend v3 v4) -> ReactiveFunction NonInitialized v1 v3
rfLoopInit (ReactiveFunction timeCont changeCont) = 
  let rf = ReactiveFunction
             (\dt -> let (changes, rf') = timeCont dt
                         (changesLeft, changesRight) = splitChanges changes
                         (changesOutput, rf'') = foldl (\(changesAcc, (ReactiveFunction _ changeCont')) change ->
                                                         let (changes', rf''') = changeCont' change
                                                         in (changesAcc ++ changes', rf'''))
                                                 ([], rf')
                                                 changesFeedback
                         (changesOutputLeft, changesOutputRight) = splitChanges changesOutput
                         changesFeedback = map ChangeRight $ changesRight ++ changesOutputRight
                         changesOutput = changesLeft ++ changesOutputLeft)
             (\change -> let (changes, rf') = changeCont change
                             (changesLeft, changesRight) = splitChanges changes
                             (changesOutput, rf'') = foldl (\(changesAcc, (ReactiveFunction _ changeCont')) change ->
                                                             let (changes', rf''') = changeCont' change
                                                             in (changesAcc ++ changes', rf'''))
                                                     ([], rf')
                                                     changesFeedback
                             (changesOutputLeft, changesOutputRight) = splitChanges changesOutput
                             changesFeedback = map ChangeRight $ changesRight ++ changesOutputRight
                             changesOutput = changesLeft ++ changesOutputLeft)


rfSigPure :: (a -> b) -> ReactiveFunction NonInitialized (RVSignal a) (RVSignal b)
rfSigPure f = 
  ReactiveFunctionNonInitialized 
    (\memory -> let rf = ReactiveFunction
                           (\_ -> ([], rf))
                           (\(ChangeSignal x) -> ([ChangeSignal $ f x], rf))
                    memory' = case memory of
                                SignalMemoryUnknown -> SignalMemoryUnknown
                                SignalMemoryValue x -> SignalMemoryValue $ f x
                in (rf, memory))

rfIntegral :: 
                                    