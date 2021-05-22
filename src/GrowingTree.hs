module GrowingTree where

import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Interface.Pure.Display

drawTree :: Int -> Float -> Picture
drawTree to t = tree 3 fracPart (intPart + 3) 10 
    where
      fracPart = snd $ properFraction (t/2)
      intPart = floor (t/2)

decFunc :: Float -> Float
decFunc x
  | x < 8 = 10 - (1.3 ** x)
  | otherwise = 10 / (1 + 1.3 ** x)

tree :: Int -> Float -> Int -> Int -> Picture
tree k t to limit
  | k < to = trunk 1 
    <> translate 0 length' (leftBranch <> rightBranch)
  | k == to = trunk t <> leaf t 
  | otherwise = blank
    where
      bottom = decFunc $ fromIntegral k
      upper = decFunc $ fromIntegral (k + 1)
      length' = 7 * bottom 

      leaf mult = translate 0 (mult * length') (color green (circleSolid (3 * bottom)))
      trunk mult = polygon 
        [(-mb, 0), (mb, 0), (mu, ml), (-mu, ml)]
            where
              ml = length' * mult
              mb = bottom
              mu = upper * mult

      branch phi = rotate phi $ tree (k + 1) t to limit
      leftBranch  = branch 30 
      rightBranch = branch (-30)
    
treeGrows = animate display' bgColor (drawTree 3)
  where
    display' = InWindow "Growing Tree" (500, 500) (200, 200) 
    bgColor = white

