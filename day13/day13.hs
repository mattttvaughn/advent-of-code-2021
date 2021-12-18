import Data.Maybe
import Data.List
import Data.List.Split

type Matrix = [[Int]]
type Point = (Int, Int)

main = do
   raw <- readFile "test.txt"
   let points = map (\l -> tuple2 $ map (\x -> read x :: Int) $ splitOn "," l ) $ takeWhile (/= "") $ lines raw
   let folds = map (drop 11) $ tail $ dropWhile (/= "") $ lines raw
   print points
   print folds

tuple2 [x0,x1] = (x0, x1)
