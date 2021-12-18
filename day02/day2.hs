import System.IO
import Data.Char(digitToInt)
import Control.Monad

main = do
    raw <- readFile "2.txt"
    let a = lines raw 
    putStrLn $ show $ product $ findLocation a
    putStrLn $ show $ product $ findLocationAim a

-- Find final coords given list like ["down 5", "up 2",...]
findLocation :: [[Char]] -> [Integer]
findLocation arr = foldl (\acc x -> zipWith (+) acc $ parseRelativeDir x) [0, 0] arr

-- (pt 2 rules) Find final coords given list like ["down 5", "up 2",...]
findLocationAim :: [[Char]] -> [Integer]
findLocationAim arr = init $ foldl (\acc x -> zipWith (+) acc $ parseRelativeAim x (acc !! 2)) [0, 0, 0] arr

-- Parse str like "down 5" or "up 2" into relative (x, y)
parseRelativeDir :: [Char] -> [Integer]
parseRelativeDir a 
    | head a == 'u' = [0, -readLastInteger a]
    | head a == 'd' = [0, readLastInteger a]
    | head a == 'f' = [readLastInteger a, 0]
    | otherwise = error ("Unknown direction type: " ++ a)

-- Parse str like "down 5" into relative (x, y, aim) using new aim method
parseRelativeAim :: [Char] -> Integer -> [Integer]
parseRelativeAim a aim
    | head a == 'u' = [0, 0, -readLastInteger a]
    | head a == 'd' = [0, 0, readLastInteger a]
    | head a == 'f' = [readLastInteger a, aim * readLastInteger a, 0]
    | otherwise = error ("Unknown direction type: " ++ a)

readLastInteger x = toInteger $ digitToInt (last x) 

