import Data.List hiding ((\\), filter, union, fromList)
import Data.List.Split
import Data.Set (Set, (\\), filter, union, fromList)
import qualified Data.Set as Set

type Matrix = [[Char]]
type Point = (Int, Int)
type Points = Set Point

main = do
   raw <- readFile "input.txt"
   let points = fromList $ map (\l -> tuple2 $ map (\x -> read x :: Int) $ splitOn "," l ) $ takeWhile (/= "") $ lines raw
   let folds = map parseFold $ map (\l -> tuple2 $ splitOn "=" l) $ map (drop 11) $ tail $ dropWhile (/= "") $ lines raw
   print $ "Pt. 1: " ++ (show $ length $ doFolds points ([head folds]))
   print $ "Pt. 2s (copy-paste points into desmos to plot): " ++ show (Set.map (\(x,y) -> (x, -1 * y)) (doFolds points folds))

doFolds :: Points -> [(Int, Int)] -> Points
doFolds ps [] = ps
doFolds ps (f:fs) = doFolds (fold ps f) fs

-- Points: the points to be folded
-- (x, y): the fold line. Only one of x or y must be present, fold over
--        a line at that location
fold :: Points -> (Int, Int) -> Points
fold ps (x,y) 
    | x /= 0 = foldLeft ps x 
    | y /= 0 = foldUp ps y
    | otherwise = error "fold cannot occur at x == 0 or y == 0"

-- "Folds" all points where x > l symmetrically across the fold line
foldLeft :: Points -> Int -> Points
foldLeft ps lx = union unfolded (Set.map (foldPointLeft lx) toFold)
    where toFold = Set.filter (\(px,py) -> px > lx) ps
          unfolded = (ps \\ toFold)

foldPointLeft lx (px,py) = (lx - (px - lx), py)

-- "Folds" all points where y > l symmetrically across the fold line
foldUp :: Points -> Int -> Points
foldUp ps ly = union unfolded (Set.map (foldPointUp ly) toFold)
    where toFold = Set.filter (\(px,py) -> py > ly) ps
          unfolded = (ps \\ toFold)

foldPointUp ly (px,py) = (px, ly - (py - ly))

tuple2 [x0,x1] = (x0, x1)

parseFold (dir, size) = if (dir == "y") then (0, read size :: Int) else (read size :: Int, 0)

