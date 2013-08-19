{-# LANGUAGE TypeOperators #-}
module Main where

import FRP.TimeFlies.SignalFunctions
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import Prelude hiding (filter)
import Stopwatch (timeTracker)
import World (WorldSVIn, WorldSVOut, GlossEvent (..), playSF)

input :: WorldSVIn :~> SVEvent ()
input = first ignore >>> cancelLeft >>> 
        filter (\e -> case e of
                        GlossEventKey (MouseButton _) Down _ -> Just ()
                        _ -> Nothing)

output :: SVSignal Double :~> WorldSVOut
output = pureSignalTransformer (color black . text . show)

main :: IO ()
main = playSF (input >>> timeTracker 0 >>> output)
