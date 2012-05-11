-- Assumed primitives:
-- mousePosition, mouseLeftClick, microphoneAudio

-- onceE :: Event a -> Event a, gives only the first occurrence
-- restE :: Event a -> Event a, gives all except the first occurrence
-- switcher :: Behavior a -> Event (Behavior a) -> Behavior a,
--             act like the given behavior, then each successive
--             behavior in the event stream

-- Assumed pre-existing functions:
-- bandpassFilter :: Behavior Double -> Behavior Double -> Behavior Double -> Behavior Double
-- (Audio, low frequency, high frequency, audio output)

import Control.FRP -- Our theoretical classic FRP module
import Control.Applicative (<$>, <*>, pure) -- fmap and applicative functor application

mouseX :: Behavior Double
mouseX = (\(x, _) -> fromIntegral x / fromIntegral screenWidth) <$> mousePosition

mouseY :: Behavior Double
mouseY = fmap (\(_, y) -> fromIntegral y / fromIntegral screenHeight) mousePosition

highFreq :: Behavior Double
highFreq = fmap (\x -> 60 * exp (x * ln (20000 / 60)) mouseX

lowFreq :: Behavior Double
lowFreq = (\y highF -> 60 * exp (y * ln (highF / 60)) <$> mouseY <*> highFreq

audioOutput :: Behavior Double
audioOutput = let filtered = bandpass microphoneInput lowFreq highFreq
                  willStopFiltering = 
                     (\evt -> filtered `switcher` 
                              fmap (const (willStartFiltering $ restE evt)) 
                              $ onceE evt)
                  willStartFiltering = 
                     (\evt -> microphoneInput `switcher` 
                              fmap (const (willStopFiltering $ restE evt)) 
                              $ onceE evt)
              in willStopFiltering mouseLeftClick

main :: IO a
main = playBehaviorAsSound audioOutput
