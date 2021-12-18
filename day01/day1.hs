import System.IO
import Control.Monad

countIncreasingPairs :: [Int] -> Int
countIncreasingPairs list = sum[1 | a <- zip list (drop 1 list), fst a < snd a]

calculateWindow3 :: [Int] -> [Int]
calculateWindow3 list = sum3 (zip3 list (drop 1 list) (drop 2 list))

sum3 = map (\(x, y, z)-> x + y + z) 

calc a = print $ show $ countIncreasingPairs a

main = do
    raw <- readFile "1.txt"
    let intArr = map read $ lines raw :: [Int]
    calc $ init intArr
    calc $ calculateWindow3 $ init intArr

