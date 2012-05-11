-- Assumed primitives:
-- arr :: (a -> b) -> SF a b
-- >>> :: SF a b -> SF b c -> SF a c
-- first :: SF a b -> SF (a, c) (b, c)
-- second :: SF a b -> SF (c, a) (c, b)
-- switch :: SF a (b, Event (SF a b)) -> SF a b
-- reactimateWithMouseAndSound :: SF (Event (), ((Int, Int), Double)) (Double) -> IO ()

-- Assumed given signal functions:
-- bandpass :: SF (Double, (Double, Double)) Double

import Control.AFRP -- Our theoretical AFRP

mouseCoordsToDoubles :: SF (Int, Int) (Double, Double)
mouseCoordsToDoubles = 
  arr (\(x, y) -> (fromIntegral x / fromIntegral screenWidth, 
                   fromIntegral y / fromIntegral screenHeight))

highFreq :: SF Double Double
highFreq = arr (\x -> 60 * exp( x * ln (20000 / 60)))

lowFreq :: SF (Double, Double) Double
lowFreq = arr (\(high, y) -> 60 * exp (y * ln (high / 60)))

lowHighFreq :: SF (Double, Double) (Double, Double)
lowHighFreq = first highFreq >>> 
              arr (\(high, y) -> (high, (high, y))) >>>
              second lowFreq

mouseCoordsToLowHighFreqs :: SF (Int, Int) (Double, Double) 
mouseCoordsToLowHighFreqs = mouseCoordsToDoubles >>> lowHighFreq

toggleFilter = 
  let filterOn = 
        switch (arr (\(evt, (mouse, audio)) -> 
                        ((audio, mouse), fmap (const filterOff) evt)) >>>
                first (second mouseCoordsToLowHighFreqs >>> bandpass))
      filterOff = switch (arr (\(evt, (_, audio)) ->
                                (audio, fmap (const filterOn) evt)))
  in filterOn

main :: IO ()
main = reactimateWithMouseAndSound toggleFilter