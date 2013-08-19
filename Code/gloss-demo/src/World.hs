{-# LANGUAGE TypeOperators #-}
module World where

import Control.Monad.Writer
import Data.Monoid
import FRP.TimeFlies.SignalFunctions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type WorldSVIn  = SVSignal (Float, Float) :^: SVEvent GlossEvent
type WorldSVOut = SVSignal Picture

type WorldSF = SF WorldSVIn WorldSVOut

data GlossEvent = GlossEventKey Key KeyState Modifiers

type World = (SFEvalState (Writer (Last Picture)) WorldSVIn WorldSVOut, Picture, Double)

toWorldTransformer :: SFEvalT WorldSVIn WorldSVOut (Writer (Last Picture)) () -> World -> World
toWorldTransformer m (st, p, t) =
  let ((_, st'), l) = runWriter $ runSFEvalT m st
      p'            = case l of
                        Last Nothing  -> p
                        Last (Just p') -> p'
  in (st', p', t)

handleEvent :: Event -> World -> World
handleEvent e = toWorldTransformer $ case e of
  EventKey k ks m pos -> do
                           push $ svRight $ svOcc $ GlossEventKey k ks m
                           update $ svLeft $ svSig pos
  EventMotion pos     -> update $ svLeft $ svSig pos

handleStep :: Float -> World -> World
handleStep td (st, p, t) =
  let t' = (realToFrac td) + t
  in toWorldTransformer (step t') (st, p, t')

playSF :: (WorldSVIn :~> WorldSVOut) -> IO ()
playSF sf = play (InWindow "TimeFlies" (500, 500) (0, 0)) 
                 white 
                 20 
                 (initSFEval (signalHandler $ tell . Last . Just) 
                             (combineSamples (sample (0, 0)) sampleEvt)
                             0
                             sf,
                  blank,
                  0)
                 (\(_, p, d) -> d `seq` p)
                 handleEvent
                 handleStep
 
