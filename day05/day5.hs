import Data.List
import Data.List.Split
import Data.Maybe
import Data.Function (on)
import Data.HashMap.Strict hiding (map, filter)

type Point = (Int, Int)
type Range = (Point, Point)

filterDiags :: [Range] -> [Range]
filterDiags x = filter (\((y,z), (y',z')) -> y == y' || z == z') x

pointsInRange :: Range -> [Point]
pointsInRange ((x,y), (x',y'))
    | x == x' = zip (repeat x) (unshittyRange y y')
    | y == y' = zip (unshittyRange x x') (repeat y)
    | otherwise = zip (unshittyRange x x') (unshittyRange y y') 

-- since haskell only handles increasing x..y ranges???
unshittyRange :: Int -> Int -> [Int]
unshittyRange x y
    | x == y = [x]
    | otherwise = [x, (x - (signum (x - y)))..y]

makePointFreqMap :: [Point] -> HashMap Point Int
makePointFreqMap ps = makePointFreqMap' ps empty
makePointFreqMap' [] map = map
makePointFreqMap' (p:ps) map = makePointFreqMap' ps (insertWith f p 1 map)
    where f new old = new + old

numOverlaps :: [Point] -> Int
numOverlaps x = foldl (\acc v -> acc + fromEnum (v > 1)) 0 (makePointFreqMap x)

main = do
   raw <- readFile "input.txt"
   let a = lines raw
   let ranges = parseRanges a
   let allPointsPt1 = concat $ map pointsInRange (filterDiags ranges)
   let allPointsPt2 = concat $ map pointsInRange ranges
   putStrLn $ "pt. 1: " ++ (show $ numOverlaps allPointsPt1)
   putStrLn $ "pt. 2: " ++ (show $ numOverlaps allPointsPt2)

parseRange :: [Char] -> Range
parseRange x = tuple2 $ map (tuple2 . map (\y -> read y :: Int) . splitOn ",") (splitOn " -> " x)

parseRanges x = map parseRange x

tuple2 [x,y] = (x,y)

