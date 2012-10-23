{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad (mapM_)                -- Monad utilities
import           Data.HList                           -- Parsed frames are HLists
import           Data.IORef                           -- Storing the table
import qualified Data.Map                        as M -- Table data structure
import           Data.Word                            -- Representation-specific
                                                      -- numbers
import           FRP.Yampa                            -- FRP
                   hiding (loop)
import           Nettle.Servers.Server                -- OpenFlow server
import           Nettle.OpenFlow                      -- OpenFlow API
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
          inputVar <- newEmptyMVar
          server <- startOpenFlowServer Nothing port
          forkIO $ let acceptLoop = do handle <- acceptSwitch server
                                       handshake handle
                                       forkIO $ let readLoop = do mMsg <- receiveFromSwitch handle
                                                                  case mMsg of
                                                                    Just (tid, msg) -> putMVar inputVar (handle, tid, msg)
                                                                    _ -> return ()
                                                                  readLoop
                                                in readLoop
                                       acceptLoop
                   in acceptLoop
          do timeRef <- newIORef 0
             readRef <- newIORef True
             reactimate (do time <- getMonoTimeAsDouble
                            writeIORef timeRef time
                            return NoEvent
                        )
                        (const $ do read <- readIORef readRef
                                    time <- getMonoTimeAsDouble
                                    oldTime <- readIORef timeRef
                                    let dt = time - oldTime
                                    if read
                                      then do threadDelay sampleTime
                                              mInput <- tryTakeMVar inputVar
                                              case mInput of
                                                Just evtTup -> do writeIORef readRef False
                                                                  return (dt, Just $ Event evtTup)
                                                _ -> return (dt, Just NoEvent)
                                      else do writeIORef readRef True
                                              return (dt, Just NoEvent))
                        (\_ out -> do case out of
                                        Event out -> out
                                        _ -> return ()
                                      return False)
                        learningSwitch

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

getPacketIn :: SF (Event (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame)) (Event (SwitchHandle EthernetFrame, TransactionID, PacketInfo EthernetFrame))
getPacketIn = arr $ mapFilterE (\(swHandle, tid, msg) -> case msg of
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

packetIn :: SF (Event ((SwitchHandle EthernetFrame, TransactionID, PacketInfo EthernetFrame), Double)) (Event TableAccumulator)
packetIn = arr $ fmap handlePacketIn

handleSCMessage :: SF (Event (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame)) (Event TableAccumulator)
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
captureTime :: SF (Event a) (Event (a, Double))
captureTime = arr (\x -> ((), x)) >>>
              first time >>>
              arr (uncurry $ flip attach)
    
-- | Handle the connection of a new switch          
switchConnect :: SF (Event (SwitchHandle EthernetFrame)) (Event (IO ()))
switchConnect = arr $ fmap (void . handshake)

-- | Accumulate a switch table, producing output messages based on the incoming functions
switchTable :: SF (Event TableAccumulator) (Event [(SwitchHandle EthernetFrame, TransactionID, CSMessage)])
switchTable = accumFilter (\acc accF -> let (out, newAcc) = accF acc
                                            out' = case out of
                                                     [] -> Nothing
                                                     _ -> Just out
                                        in (newAcc, out'))  M.empty


-- | Produce an event with the rule-cleaning accumulator.
cleanRules :: Double -> SF () (Event TableAccumulator)
cleanRules t = constant ((), ()) >>> second (repeatedly t cleanRulesAcc) >>> first time >>> arr (uncurry $ flip attach) >>> arr (fmap $ uncurry ($))
  where 
    cleanRulesAcc :: Double -> TableAccumulator
    cleanRulesAcc nowT table = cleanTable t nowT table

sendCSMessage :: SF (Event [(SwitchHandle EthernetFrame, TransactionID, CSMessage)]) (Event (IO ()))
sendCSMessage = arr $ fmap $ mapM_ (\(handle, tid, msg) -> sendToSwitch handle (tid, msg))

learn :: SF (Event (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame)) (Event (IO ()))
learn = arr (\x -> ((), x)) >>> first (cleanRules tableTime) >>> second handleSCMessage >>> arr (uncurry $ mergeBy (>>)) >>> switchTable >>> sendCSMessage

learningSwitch :: SF (Event (SwitchHandle EthernetFrame, TransactionID, SCMessage EthernetFrame)) (Event (IO ()))
learningSwitch = learn
