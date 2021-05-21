module TrafficSystem where
    {-

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text

-- | Lights of different colors.
greenCircle :: Picture -> Picture
greenCircle = colored green

yellowCircle :: Picture -> Picture
yellowCircle = colored yellow

redCircle :: Picture -> Picture
redCircle = colored red

-- | Big light.
carCircle :: Picture
carCircle = solidCircle 1

-- | Small light.
smallCircle :: Picture
smallCircle = solidCircle 0.7

-- | Make colors of lights dim.
dimGreenLight :: Picture -> Picture
dimGreenLight = colored (RGBA 0 0.3 0 1)

dimYellowLight :: Picture -> Picture
dimYellowLight = colored (RGBA 0.3 0.3 0 1)

dimRedLight :: Picture -> Picture
dimRedLight = colored (RGBA 0.3 0 0 1)

-- | Brights colors for big lights.
greenCarLight :: Picture
greenCarLight = translated 0 (-2.2) (greenCircle carCircle)

yellowCarLight :: Picture
yellowCarLight = translated 0 0 (yellowCircle carCircle)

redCarLight :: Picture
redCarLight = translated 0 2.2 (redCircle carCircle)

-- | Dim colors for big lights.
dimGreenCarLight :: Picture
dimGreenCarLight = dimGreenLight greenCarLight

dimYellowCarLight :: Picture
dimYellowCarLight = dimYellowLight yellowCarLight

dimRedCarLight :: Picture
dimRedCarLight = dimRedLight redCarLight

-- | Bright colors for small lights.
greenSmallLight :: Picture
greenSmallLight = translated 0 (-0.8) (greenCircle smallCircle)

redSmallLight :: Picture
redSmallLight = translated 0 (0.8) (redCircle smallCircle)

-- | Dim colors for small lights.
dimGreenSmallLight :: Picture
dimGreenSmallLight = dimGreenLight greenSmallLight

dimRedSmallLight :: Picture
dimRedSmallLight = dimRedLight redSmallLight

-- | Frame for traffic lights for cars.
carFrame :: Picture
carFrame = rectangle 2.5 7.6

-- | Frame for small traffic lights.
smallFrame :: Picture
smallFrame = rectangle 1.6 3.4

-- States of traffic lights for cars
data CarStates = CarR | CarY | CarG | CarRY

-- display the state of a traffic light for cars
carTrafficLights :: CarStates -> Picture
carTrafficLights CarR = carFrame <> redCarLight <> dimYellowCarLight <> dimGreenCarLight
carTrafficLights CarY = carFrame <> dimRedCarLight <> yellowCarLight <> dimGreenCarLight
carTrafficLights CarG = carFrame <> dimRedCarLight <> dimYellowCarLight <> greenCarLight
carTrafficLights CarRY = carFrame <> redCarLight <> yellowCarLight <> dimGreenCarLight

-- choose the state of a traffic light for cars depending on time
carController :: Double -> Picture
carController t
  | 0 <= t' && t' < 3 = carTrafficLights CarG
  | 3 == t' = carTrafficLights CarY
  | 4 <= t' && t' < 7 = carTrafficLights CarR
  | t' == 7 = carTrafficLights CarRY
  where
    t' = floor t `mod` 8

-- States of small traffic lights
data SmallStates = NCarR | BlinkingG | NCarG

-- generic construction of small traffic lights
smallTrafficLightsHelp :: Char -> Double -> SmallStates -> Picture
smallTrafficLightsHelp icon offset = translated offset 0 . smallTrafficLights frame
  where
    frame = translated 0 (-0.8) (lettering (pack [icon])) <> smallFrame

-- traffic lights for pedestrians
pedestrianTrafficLights :: SmallStates -> Picture
pedestrianTrafficLights = smallTrafficLightsHelp '\x1F6B6' 3

-- traffic lights for bycicles
bycicleTrafficLights :: SmallStates -> Picture
bycicleTrafficLights = smallTrafficLightsHelp '\x1F6B2' 6

-- display the state of a traffic light
smallTrafficLights :: Picture -> SmallStates -> Picture
smallTrafficLights frame NCarR = frame <> redSmallLight <> dimGreenSmallLight
smallTrafficLights frame BlinkingG = frame <> dimRedSmallLight <> dimGreenSmallLight
smallTrafficLights frame NCarG = frame <> dimRedSmallLight <> greenSmallLight
  
-- choose the state of a traffic light depending on time
smallController :: (SmallStates -> Picture) -> Double -> Picture
smallController lightsType t
  | 0 <= t' && t' < 4 = lightsType NCarR
  | 4 <= t' && t' < 7 = lightsType NCarG
  | t' == 7 = if firstDigit `mod` 2 == 0 
                  then lightsType BlinkingG
                  else lightsType NCarG
    where
      decimalPartOfT = snd $ properFraction t
      firstDigit = floor (decimalPartOfT * 10)
      t' = floor t `mod` 8
  
-- entry point
main :: IO ()
main = animationOf $ 
  smallController bycicleTrafficLights
  <> smallController pedestrianTrafficLights
  <> carController
-}
