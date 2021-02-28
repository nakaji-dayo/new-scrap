module Q10M where

import           Data.Maybe (catMaybes)
type Position = (Int, [Int])

data Move = U | D | L | R

encode :: Position -> Int
encode = undefined

size = 3

moves :: Position -> [Move]
moves p@(p0, cs) =
  let (y, x) = divMod p0 3
  in catMaybes [ if y < (size - 1) then Just U else Nothing
               , if y > 0 then Just D else Nothing
               , if x /= (size - 1) then Just L else Nothing
               , if x /= 0 then Just R else Nothing
               ]

move :: Position -> Move -> Position
move (p0, cs) m =
  let d = case m of
        U -> 3
        D -> -3
        L -> 1
        R -> -1
  in (p0 + d, swapl p0 d cs)


solved :: Position -> Bool
solved = undefined

swapl a b xs =
  let (r, ea, eb) = foldr f ([], ea, eb) (zip [0..] xs)
  in r
  where
    f (i, x) (acc, aa, ab)
      | i == a = (ab:acc, x, ab)
      | i == b = (aa:acc, aa, x)
      | otherwise = (x:acc, aa, ab)

swapl' j k xs = xs1 ++ y:xs3 ++ x:xs4
  where
    (xs1, x:xs2) = splitAt j xs
    (xs3, y:xs4) = splitAt (k - j - 1) xs2

testswap = []
