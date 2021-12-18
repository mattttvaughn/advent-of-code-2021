import Debug.Trace
import System.IO
import Data.Char(digitToInt)
import Control.Monad

main = do
    raw <- readFile "test.txt"
    let a = map (\x -> map (\y -> digitToInt y) x) (lines raw)
    putStrLn $ show $ (fromBinary $ epsilon a) * (fromBinary $ gamma a)
    putStrLn $ show $ calcOx (transpose a) []

fromBinary :: [Int] -> Int
fromBinary x = foldl (\acc y -> acc + (2 ^ (snd y)) * (fst y)) 0 (zip (reverse x) [0,1..])

gamma :: [[Int]] -> [Int]
gamma input = map mcb (transpose input)
epsilon input = map (\x -> if x == 1 then 0 else 1) (gamma input)

mcb :: [Int] -> Int
mcb input = (sum input) `div` ((length input) `div` 2)

lcb input = (if ((mcb input) == 1) then 0 else 1)

-- Reduces each [Int] to it's most common bits, return as an [Int]
calcOx :: [[Int]] -> [Int] -> [Int]
calcOx [] acc = acc
calcOx (x:xs) acc = calcOx xs (acc ++ [mcb x])

transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
