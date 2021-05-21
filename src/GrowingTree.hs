module GrowingTree where

import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Interface.Pure.Display

drawTree :: Int -> Float -> Picture
drawTree to t = tree 3 fracPart (intPart + 3)
    where
      fracPart = snd $ properFraction (t/2)
      intPart = floor (t/2)

decFunc :: Float -> Float
decFunc x
  | x < 8 = 10 - (1.3 ** x)
  | otherwise = 10 / (1 + 1.3 ** x)

tree :: Int -> Float -> Int -> Picture
tree k t to 
  | k < to = trunk 1 
    <> translate 0 length' (leftBranch <> rightBranch)
    <> leaf 1 
  | k == to = trunk t <> leaf t 
  | otherwise = blank
    where
      bottom = decFunc $ fromIntegral k
      upper = decFunc $ fromIntegral (k + 1)
      length' = 7 * bottom 

      leaf mult = translate 0 (mult * length') (color green (circleSolid (mult * upper)))
      trunk mult = polygon 
        [(-mb, 0), (mb, 0), (mu, ml), (-mu, ml)]
            where
              ml = length' * mult
              mb = bottom * mult
              mu = upper * mult

      leftBranch  = rotate 30 (tree (k + 1) t to)
      rightBranch = rotate (-30) (tree (k + 1) t to)
    
treeGrows = animate display' bgColor (drawTree 3)
  where
    display' = InWindow "Growing Tree" (500, 500) (200, 200) 
    bgColor = white

