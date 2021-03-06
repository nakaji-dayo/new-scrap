module Q10M where

import qualified Data.IntSet as S
import           Data.List   (foldl', sort)
import           Data.Maybe  (catMaybes)
import           Debug.Trace

type Position = (Int, [Int])

data Move = U | D | L | R
  deriving (Show, Eq)

type Path = ([Move], Position)
type Frontier = [Path]

size = 3

moves :: Position -> [Move]
moves (p0, cs) =
  let (y, x) = divMod p0 3
  in catMaybes [ if y < (size - 1) then Just U else Nothing
               , if y > 0 then Just D else Nothing
               , if x /= (size - 1) then Just L else Nothing
               , if x /= 0 then Just R else Nothing
               ]

move :: Position -> Move -> Position
move p@(p0, cs) m =
  let d = case m of
        U -> 3
        D -> -3
        L -> 1
        R -> -1
  in (p0 + d, swapl p0 (p0 + d) cs)


solved :: Position -> Bool
-- solved (p0, _) = p0 == 8
-- solved (p0, cs) = p0 == 8 && n0 == sort n0
--   where n0 = filter (/= 0) cs
solved (_, p) = p == [1,2,3,4,5,6,7,8,0]

swapl a b xs =
  let (r, ea, eb) = foldr f ([], ea, eb) (zip [0..] xs)
  in r
  where
    f (i, x) (acc, aa, ab)
      | i == a = (ab:acc, x, ab)
      | i == b = (aa:acc, aa, x)
      | otherwise = (x:acc, aa, ab)

solve p = bfs S.empty [([], p)] []

bfs :: S.IntSet -> Frontier -> Frontier -> Maybe [Move]
bfs ps [] [] = Nothing
bfs ps [] mqs = bfs ps mqs []
bfs ps ((ms, p):mps) mqs
  | solved p = Just $ reverse ms
  | encode p `S.member` ps = bfs ps mps mqs
  | otherwise = bfs (S.insert (encode p) ps) mps (succs (ms, p) ++ mqs)

succs (ms, p) = [(m:ms, move p m) | m <- moves p]

encode :: Position -> Int
encode (_, cs) = fst $ foldl' (\(r, n) x -> (r + x*n, n*10)) (0, 1) cs
-- todo...
