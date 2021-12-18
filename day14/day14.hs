import Data.Maybe
import Data.List 
import Data.List.Split
import Data.Map (Map, fromList)
import qualified Data.Map as Map

main = do
   raw <- readFile "input.txt"
   let polySeq = head $ lines raw
   let seqMap = fromList $ map tuple2 $ map (splitOn " -> ") $ drop 2 $ lines raw
   print $ freqDiff $ replace seqMap polySeq 10
   -- TODO: too slow for part 2... will have to refactor to be smarter

-- Return count of most freq element minus least freq element
freqDiff :: [Char] -> Int
freqDiff a = (maximum pairs) - (minimum pairs)
    where pairs = Map.elems $ toFreq a

toFreq :: [Char] -> Map Char Int
toFreq x = fromList $ map (\x -> (head x, length x)) (group $ sort x)

replace m s 0 = s
replace m s n = replace m (replacePairs m s) (n - 1)

replacePairs :: Map String String -> String -> String
replacePairs m s = (concat $ map (replacePair m) $ window2 s) ++ [last s]

replacePair :: Map String String -> String -> String
replacePair m s = [head s] ++ replacer 
    where replacer = fromMaybe "" $ Map.lookup s m

tuple2 [x,y] = (x,y)

-- Sliding window size 2, https://twitter.com/GabriellaG439/status/701460899589017600
window2 xs = init $ Data.List.transpose (take 2 (tails xs))
