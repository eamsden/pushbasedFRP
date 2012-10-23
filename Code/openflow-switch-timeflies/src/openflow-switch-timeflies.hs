{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad (mapM_)                -- Monad utilities
import           Data.HList                           -- Parsed frames are HLists
import           Data.IORef                           -- Storing the table
import qualified Data.Map                        as M -- Table data structure
import           Data.Word                            -- Representation-specific
                                                      -- numbers
import           FRP.TimeFlies.SignalFunctions        -- FRP
                   hiding (loop)
import           GHC.Event hiding (step)              -- GHC event loop
import           Nettle.OpenFlow                      -- OpenFlow API
import           Nettle.Servers.EventServer           -- OpenFlow event-based server
import           System.Clock (getTime,               -- A monotonic clock
                               Clock(Monotonic),
                               TimeSpec,
                               sec,
                               nsec)
import           System.Environment (getArgs)                               

import qualified Data.List as L (filter)
import Prelude hiding (filter)

-- | The port for the server to listen on.
port :: Word16
port = 6633

-- | The amount of time a rule should persist in the table
tableTime :: Double
tableTime = 5.0

-- | Entry point
main :: IO ()
main = do (argS:_) <- getArgs
          let sampleTime = read argS
          time <- getMonoTimeAsDouble
          reactiveRef <- newIORef $ initSFEval (eventHandler id) (combineSamples sampleEvt sampleEvt) time learningSwitch
          ofEventManager <- 
            openFlowEventManager Nothing port (\handle -> do rState <- readIORef reactiveRef
                                                             ((), rState') <- runSFEvalT (push $ svLeft $ svOcc handle) rState
                                                             writeIORef reactiveRef rState')
                                 (\((tid, msg), handle) -> do rState <- readIORef reactiveRef
                                                              ((), rState') <- runSFEvalT (push $ svRight $ svOcc (handle, tid, msg)) rState
                                                              writeIORef reactiveRef rState')
          let evtMgr = getEventManager ofEventManager
              sample = void $ registerTimeout evtMgr sampleTime $ do rState <- readIORef reactiveRef
                                                                     time <- getMonoTimeAsDouble
                                                                     ((), rState') <- runSFEvalT (step time) rState
                                                                     writeIORef reactiveRef rState'
                                                                     sample
          sample 
          loop $ evtMgr      

-- | Get the monotonic time as a Double number of seconds
getMonoTimeAsDouble :: IO Double
getMonoTimeAsDouble =
  fmap (\ts -> fromIntegral (sec ts) + 1.0e-9 * fromIntegral (nsec ts)) $
    getTime Monotonic

-- | An entry in the switch table
data SwitchEntry =
  SwitchEntry { 
    swEntryHandle         :: SwitchHandle EthernetFrame,
    swEntryAddress        :: EthernetAddress,
    swEntryPort           :: PortID,
    swEntryCurrentMatches :: [Match],
    swEntryTime           :: Double
  }
  

-- | A key for the switch table is a pair of a switch ID and an Ethernet address.
type SwitchTableKey = (SwitchID, EthernetAddress)
type SwitchTable = M.Map SwitchTableKey SwitchEntry
  
-- | Add a rule-match to a switch entry
addMatchToSwitchEntry :: Match -> SwitchEntry -> SwitchEntry
addMatchToSwitchEntry m swe =
  swe { swEntryCurrentMatches = m : (swEntryCurrentMatches swe) }

-- | Add a rule match to the entry stored at a particular key in a switch table
addMatchToTable :: Match -> SwitchTableKey -> SwitchTable -> SwitchTable
addMatchToTable = M.adjust . addMatchToSwitchEntry

-- | Add an address->port mapping to the switch table
addAddressPortToTable ::    SwitchHandle EthernetFrame
                         -> EthernetAddress
                         -> PortID
                         -> Double
                         -> SwitchTable
                         -> SwitchTable
addAddressPortToTable handle address port time =
  M.insert (handle2SwitchID handle, address)
           (SwitchEntry handle address port [] time)

-- | Function type to modify a table and produce messages to the switch
type TableAccumulator = SwitchTable -> ([(SwitchHandle EthernetFrame, TransactionID, CSMessage)], SwitchTable)

getPacketIn :: SVEvent (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame) :~> SVEvent (SwitchHandle EthernetFrame, TransactionID, PacketInfo EthernetFrame)
getPacketIn = filter (\(swHandle, tid, msg) -> case msg of
                                                 PacketIn pi -> Just (swHandle, tid, pi)
                                                 _ -> Nothing)

-- | Handle a packet in event
handlePacketIn :: ((SwitchHandle EthernetFrame, TransactionID, PacketInfo EthernetFrame), Double) -> TableAccumulator
handlePacketIn ((swHandle, transID, packetInfo), time) table =
  let header       = hHead $ enclosedFrame packetInfo
      src          = sourceMACAddress header
      dst          = destMACAddress header
      inPort       = receivedOnPort packetInfo
      swId         = handle2SwitchID swHandle
      (dropMsgs, dropTable) = dropEntry (handle2SwitchID swHandle, src) table 
      newTable     = addAddressPortToTable swHandle src inPort time table
      mOutPort     = fmap swEntryPort $ M.lookup (handle2SwitchID swHandle, dst) newTable
      packetOut    = PacketOut .
                     PacketOutRecord (maybe (Right $ packetData packetInfo)
                                            Left
                                            (bufferID packetInfo))
                                     (Just inPort)
      (matchMsgs, alterTable) 
                   = maybe ([(swHandle, transID, packetOut flood)], id)
                           (\outPort -> let matchForward = Match Nothing
                                                           (Just src) (Just dst)
                                                           Nothing Nothing
                                                           Nothing Nothing
                                                           Nothing
                                                           (IPAddress 0, 0)
                                                           (IPAddress 0, 0)
                                                           Nothing Nothing
                                            matchReverse = Match Nothing
                                                           (Just dst) (Just src)
                                                           Nothing Nothing
                                                           Nothing Nothing
                                                           Nothing
                                                           (IPAddress 0, 0)
                                                           (IPAddress 0, 0)
                                                           Nothing Nothing
                                            flowForward = FlowMod $ 
                                                            AddFlow matchForward 0
                                                                   (sendOnPort outPort)
                                                                   0 Permanent Permanent
                                                                   False
                                                                   (bufferID packetInfo)
                                                                   False
                                            flowReverse = FlowMod $
                                                            AddFlow matchReverse 0
                                                            (sendOnPort inPort)
                                                            0 Permanent Permanent
                                                            False Nothing False
                                            packets = (maybe [packetOut $
                                                               sendOnPort outPort]
                                                             (const []) $
                                                             bufferID packetInfo)
                                                      ++ [flowForward, flowReverse]
                                            msgs = map (\p -> (swHandle, 0, p)) packets
                                            tableMod = (addMatchToTable matchReverse (swId, src) .
                                                        addMatchToTable matchForward (swId, src) .
                                                        addMatchToTable matchReverse (swId, dst) .
                                                        addMatchToTable matchForward (swId, dst))
                                        in (msgs, tableMod))
                     mOutPort
  in (dropMsgs ++ matchMsgs, alterTable newTable)

packetIn :: SVEvent ((SwitchHandle EthernetFrame, TransactionID, PacketInfo EthernetFrame), Double) :~> SVEvent TableAccumulator
packetIn = pureEventTransformer handlePacketIn

handleSCMessage :: SVEvent (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame) :~> SVEvent TableAccumulator
handleSCMessage = getPacketIn >>> captureTime >>> packetIn

-- | Alter a table and generate messages to remove expired entries and rules
cleanTable :: Double -> Double -> TableAccumulator
cleanTable tableInterval nowTime table =
  let timedOut = 
        map fst $ L.filter 
          (\(key, entry) -> 
            nowTime - swEntryTime entry > tableInterval) $
        M.toList table
  in foldl (\(act, tb) key -> let (newAct, newTab) = dropEntry key tb
                              in (act ++ newAct, newTab))
           ([], table) timedOut
                                                             
-- | Drop an entry from the switch table, generating messages to remove the rules based on it.
dropEntry :: SwitchTableKey -> TableAccumulator
dropEntry key table =
     let mEntry = M.lookup key table
         newTable = M.delete key table
         action = maybe [] (\entry -> concatMap (deleteMatch $ swEntryHandle entry)
                                      (swEntryCurrentMatches entry))
                        mEntry
     in (action, newTable)
  where deleteMatch handle match = [(handle, 0, FlowMod $ DeleteFlows match Nothing)]
            
-- | Capture the occurrence time of an event.
captureTime :: SVEvent a :~> SVEvent (a, Double)
captureTime = uncancelLeft >>>
              first time >>>
              capture
    
-- | Handle the connection of a new switch          
switchConnect :: SVEvent (SwitchHandle EthernetFrame) :~> SVEvent (IO ())
switchConnect = pureEventTransformer (void . handshake)

-- | Accumulate a switch table, producing output messages based on the incoming functions
switchTable :: SVEvent TableAccumulator :~> SVEvent (SwitchHandle EthernetFrame, TransactionID, CSMessage)
switchTable = accumulateList (flip ($)) M.empty

-- | Produce an event with the given value at the given time interval
every :: Double -> a -> SVEmpty :~> SVEvent a
every t x = let e = switch (after t x >>> copy >>> second (pureEventTransformer $ const e))
            in e

-- | Produce an event with the rule-cleaning accumulator.
cleanRules :: Double -> SVEmpty :~> SVEvent TableAccumulator
cleanRules t = ignore >>> copy >>> second (every t cleanRulesAcc) >>> first time >>> capture >>> pureEventTransformer (uncurry ($))
  where 
    cleanRulesAcc :: Double -> TableAccumulator
    cleanRulesAcc nowT table = cleanTable t nowT table

sendCSMessage :: SVEvent (SwitchHandle EthernetFrame, TransactionID, CSMessage) :~> SVEvent (IO ())
sendCSMessage = pureEventTransformer (\(handle, tid, msg) -> sendToSwitch handle (tid, msg))

learn :: SVEvent (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame) :~> SVEvent (IO ())
learn = uncancelLeft >>> first (cleanRules tableTime) >>> second handleSCMessage >>> union >>> switchTable >>> sendCSMessage


     
learningSwitch :: (SVEvent (SwitchHandle EthernetFrame) :^: SVEvent (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame)) :~> SVEvent (IO ())
learningSwitch = first switchConnect >>> second learn >>> union
