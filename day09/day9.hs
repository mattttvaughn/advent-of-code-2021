import Data.Maybe
import Data.List
import Data.List.Split
import Data.List (sortBy)
import Data.Function (on)

type Matrix = [[Int]]
type Point = (Int, Int)

main = do
   raw <- readFile "input.txt"
   let charMatrix = map (\x -> filter (/= "") x) $ map (splitOn "") $ lines raw
   let inputMatrix = map (\xs -> map (\x -> read x :: Int) xs) charMatrix
   print $ solveP1 inputMatrix
   print $ solveP2 inputMatrix

solveP1 m = sum $ map (+ 1) $ map (depth m) $ findLowPoints m 
solveP2 m = product $ take 3 $ reverse $ sort $ map (\p -> length $ findBasin m [p]) $ findLowPoints m

-- Find a set of points making up a basin in [m] surrounding [lps]
findBasin :: Matrix -> [Point] -> [Point]
findBasin m lps = findBasin' m lps False
findBasin' m ps True = ps 
findBasin' m ps hasFinished = findBasin' m ps' (length ps' == length ps)
    where ps' = expandBasin m ps

-- Add all neighbors of [ps] where depth depth p < depth neighbor < 9
expandBasin :: Matrix -> [Point] -> [Point]
expandBasin m ps = union ps $ foldl union [] $ map (\p -> validNeighbors m p) ps

validNeighbors :: Matrix -> Point -> [Point]
validNeighbors m p = filter (\n -> depthP < (depth m n) && (depth m n) < 9) $ neighbors m p
    where depthP = depth m p

-- returns the depth of each low point in a matrix
findLowPoints :: Matrix -> [Point]
findLowPoints m = filter (isLowPoint m) coords
    where coords = [(x,y) | x <- [0..(length $ m !! 0) - 1], y <- [0..length m - 1]]

isLowPoint :: Matrix -> Point -> Bool
isLowPoint m p = all (> depth m p) (map (depth m) (neighbors m p))

depth m p = m !! snd p !! fst p

-- All available depths in a matrix immediately up|down|left|right of a point
neighbors :: Matrix -> Point -> [Point]
neighbors m (x, y) = catMaybes [up, down, left, right]
    where up = getMaybe m (x, y + 1)
          down = getMaybe m (x, y - 1)
          left = getMaybe m (x - 1, y)
          right = getMaybe m (x + 1, y)

getMaybe :: Matrix -> Point -> Maybe Point
getMaybe m (x, y) = if (isInRangeY && isInRangeX) then Just (x, y) else Nothing
    where isInRangeY = y >= 0 && y < (length m)
          isInRangeX = x >= 0 && x < (length $ head m)
