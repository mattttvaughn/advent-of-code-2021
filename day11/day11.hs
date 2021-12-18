import Data.Maybe
import Data.List
import Data.List.Split

type Matrix = [[Int]]
type Point = (Int, Int)

main = do
   raw <- readFile "input.txt"
   let charMatrix = map (filter (/= "")) $ map (splitOn "") $ lines raw
   let m = map (map (\x -> read x :: Int)) charMatrix
   print $ "Pt. 1: " ++ show (execCycles m 100)
   print $ "Pt. 2: " ++ show (firstSync m)

incPass :: Matrix -> Matrix
incPass m = map (map succ) m

execCycles :: Matrix -> Int -> Int
execCycles m n = execCycles' m n 0
execCycles' m 0 flashes = flashes
execCycles' m cycles flashes = execCycles' m' (cycles - 1) (flashes + flashes')
    where (m', flashes') = flashPass m

firstSync :: Matrix -> Int
firstSync m = firstSync' m 0
firstSync' m step = if (isDone) then (step + 1) else firstSync' m' (step + 1)
    where (m', _) = flashPass m
          isDone = all (\p -> valueAt m' p == 0) $ toPoints m'

flashPass :: Matrix -> (Matrix, Int)
flashPass m = (iterate flash (inc'd, 0)) !! 30
    where inc'd = incPass m

-- Given any numbers valued >9:
--   change the >9s to 0s
--   +1 all adjacent points to flashed points
flash :: (Matrix, Int) -> (Matrix, Int)
flash (m, acc) = (flashed, acc + count (\p -> valueAt m p > 9) (toPoints m))
    where x' x neighborFlashes
             | x > 9 || x == 0 = 0
             | otherwise = x + neighborFlashes
          nearFlashes p = count (\n -> valueAt m n > 9) (neighbors m p)
          flashed = mapPoints (\p -> x' (valueAt m p) (nearFlashes p)) m

count fxn xs = length $ filter fxn xs

-- Applies a function to each point in a matrix
mapPoints :: (Point -> Int) -> Matrix -> Matrix
mapPoints f m = chunksOf (length $ m !! 0) $ map (f) ps
    where ps = toPoints m

toPoints m = [(x,y) | x <- [0..(width m - 1)], y <- [0..(length m - 1)]]

width m = length $ m !! 0 
valueAt m p = m !! snd p !! fst p

-- Returns all points in a matrix adjacent to a point (including diagonals) 
neighbors :: Matrix -> Point -> [Point]
neighbors m (x, y) = catMaybes maybes
    where maybes = map (\(x',y') -> getMaybe m (x+x', y+y')) relatives
          relatives = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]

getMaybe :: Matrix -> Point -> Maybe Point
getMaybe m (x, y) = if (isInRangeY && isInRangeX) then Just (x, y) else Nothing
    where isInRangeY = y >= 0 && y < (length m)
          isInRangeX = x >= 0 && x < (length $ head m)
