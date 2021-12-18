import Data.List 
import Data.List.Split

main = do
   raw <- readFile "test.txt"
   let parsed = map (\x -> read x :: Int) (splitOn "," raw)
   print $ solve (\a b -> abs (a - b)) parsed
   print $ solve (\a b -> sum [0..(abs (a - b))]) parsed

-- for every n
solve costFxn x = minimum $ map (\n -> sum $ map (\y -> costFxn y n) x) range
    where range = [(minimum x)..(maximum x)]

