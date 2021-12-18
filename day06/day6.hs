import Data.List hiding (union)
import Data.List.Split
import Data.IntMap.Strict (IntMap, (!), mapWithKey, elems, empty, insertWith, fromList, union)
import qualified Data.IntMap.Strict as IntMap 

main = do
   raw <- readFile "input.txt"
   let parsed = map (\x -> read x :: Int) (splitOn "," raw)
   print $ fishCount parsed 80
   print $ fishCount parsed 256

fishCount n fs = sum $ elems $ afterXDays (toFreq n) fs

toFreq :: [Int] -> IntMap Int
toFreq x = fillMap $ fromList $ map (\x -> (head x, length x)) (group $ sort x)

-- Add any missing keys from [0..8], using value 0 as their value
fillMap x = union x $ fromList $ zip [0..8] (repeat 0)

afterXDays :: IntMap Int -> Int -> IntMap Int
afterXDays fMap 0 = fMap
afterXDays fMap day = afterXDays (mapWithKey (\k _ -> updateValue k fMap) fMap) (day - 1)

updateValue :: Int -> IntMap Int -> Int
updateValue k oldValues
    | k `elem` [0..5] || k == 7 = oldValues ! (k + 1)
    | k == 6 = (oldValues ! 0) + (oldValues ! 7)
    | k == 8 = oldValues ! 0
    | otherwise = error $ "Invalid index: " ++ (show k)
