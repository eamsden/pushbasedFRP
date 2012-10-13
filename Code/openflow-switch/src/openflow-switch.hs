module Main where

import           Control.Monad (mapM_)                -- Monad utilities
import           Data.HList                           -- Parsed frames are HLists
import           Data.IORef                           -- Storing the table
import qualified Data.Map                        as M -- Table data structure
import           Data.Word                            -- Representation-specific numbers
import           GHC.Event                            -- GHC event loop
import           Nettle.OpenFlow                      -- OpenFlow API
import           Nettle.Servers.EventServer           -- OpenFlow event-based server
import           System.Clock (getTime,               -- A monotonic clock
                               Clock(Monotonic),
                               TimeSpec,
                               sec,
                               nsec)

port :: Word16
port = 6633

tableTime :: Double
tableTime = 5.0

cleanInterval :: Int
cleanInterval = 10000

main :: IO ()
main = do tableRef <- newIORef M.empty
          ofEventManager <- 
            openFlowEventManager Nothing port handleSwitchConnect
                                 (handleSCMessage tableRef)
          let evtMgr = getEventManager ofEventManager
          cleanTable cleanInterval tableTime tableRef evtMgr
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
  

type SwitchTableKey = (SwitchID, EthernetAddress)
type SwitchTable = M.Map SwitchTableKey SwitchEntry
  
addMatchToSwitchEntry :: Match -> SwitchEntry -> SwitchEntry
addMatchToSwitchEntry m swe =
  swe { swEntryCurrentMatches = m : (swEntryCurrentMatches swe) }

addMatchToTable :: Match -> SwitchTableKey -> SwitchTable -> SwitchTable
addMatchToTable = M.adjust . addMatchToSwitchEntry

addAddressPortToTable ::    SwitchHandle EthernetFrame
                         -> EthernetAddress
                         -> PortID
                         -> Double
                         -> SwitchTable
                         -> SwitchTable
addAddressPortToTable handle address port time =
  M.insert (handle2SwitchID handle, address)
           (SwitchEntry handle address port [] time)

handlePacketIn ::    SwitchHandle EthernetFrame
                  -> TransactionID
                  -> PacketInfo EthernetFrame
                  -> IORef SwitchTable
                  -> IO ()
handlePacketIn swHandle transID packetInfo tableRef =
  do time <- getMonoTimeAsDouble
     let header       = hHead $ enclosedFrame packetInfo
         src          = sourceMACAddress header
         dst          = destMACAddress header
         inPort       = receivedOnPort packetInfo
     dropEntry tableRef (handle2SwitchID swHandle, src)
     table <- readIORef tableRef
     let newTable     = addAddressPortToTable swHandle src inPort time table
     writeIORef tableRef newTable
     let mOutPort  = fmap swEntryPort $ M.lookup (handle2SwitchID swHandle, dst) newTable
         packetOut = PacketOut .
                     PacketOutRecord (maybe (Right $ packetData packetInfo)
                                            Left
                                            (bufferID packetInfo))
                                     (Just inPort)
     maybe (sendToSwitch swHandle (transID, packetOut flood))
           (\outPort -> do let matchForward = Match Nothing
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
                               swId = handle2SwitchID swHandle
                           mapM_ (sendToSwitch swHandle) $
                             map (\x -> (transID, x)) packets
                           modifyIORef tableRef
                             (addMatchToTable matchReverse (swId, src) .
                              addMatchToTable matchForward (swId, src) .
                              addMatchToTable matchReverse (swId, dst) .
                              addMatchToTable matchForward (swId, dst))
           )
           mOutPort

handleSCMessage :: IORef SwitchTable -> SCMessageCallback EthernetFrame
handleSCMessage tableRef ((tid, scmessage), swHandle) =
  case scmessage of
    PacketIn info -> handlePacketIn swHandle tid info tableRef
    _ -> return ()
    
cleanTable :: Int -> Double -> IORef SwitchTable -> EventManager -> IO ()
cleanTable cleanTime tableInterval tableRef mgr =
  void $ registerTimeout mgr cleanTime cleanTableCB
  where cleanTableCB = do table <- readIORef tableRef
                          time <- getMonoTimeAsDouble
                          let timedOut = 
                                map fst $ filter 
                                  (\(key, entry) -> 
                                     time - swEntryTime entry > tableInterval) $
                                  M.toList table
                          mapM_ (dropEntry tableRef) timedOut
                          void $ registerTimeout mgr cleanTime cleanTableCB

dropEntry :: IORef SwitchTable -> SwitchTableKey -> IO ()
dropEntry tableRef key =
  do table <- readIORef tableRef
     let mEntry = M.lookup key table
     maybe (return ()) (\entry -> mapM_ (deleteMatch $ swEntryHandle entry)
                                        (swEntryCurrentMatches entry))
           mEntry
     writeIORef tableRef $ M.delete key table
  where deleteMatch handle match =
          do sendToSwitch handle (0, FlowMod $ DeleteFlows match Nothing)

handleSwitchConnect :: SwitchHandle EthernetFrame -> IO ()
handleSwitchConnect handle =
  do void $ handshake handle
