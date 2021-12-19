import Data.Ord
import Data.List 
import Data.List.Split
import Data.Map (Map, (!), fromList)
import qualified Data.Map as Map

type PairFreqs = [(String,Int)]
type PairFreq = (String,Int)
type ReplacementMap = Map String String

main = do
   raw <- readFile "input.txt"
   let polyPairMap = toFreq $ window2 $ head $ lines raw
   let seqMap = fromList $ map tuple2 $ map (splitOn " -> ") $ drop 2 $ lines raw
   -- minus one because of the end characters, which are misrepresented
   -- since we halve the characters in our "bigram" count we're using.
   -- I think this may differ depending on whether first/last chars are the
   -- most frequent
   print $ (freqDiff $ replace seqMap polyPairMap 10) - 1 
   print $ (freqDiff $ replace seqMap polyPairMap 40) - 1

charFreq :: PairFreqs -> [(Char, Int)]
charFreq freqs = mergePairs $ concat $ map (\(ss,freq) -> [(head ss, freq), (last ss, freq)]) freqs

freqDiff :: PairFreqs -> Int
freqDiff freqs = maximum (map snd charFreqs) `div` 2 - minimum (map snd charFreqs) `div` 2
    where charFreqs = charFreq freqs

toFreq :: (Ord a) => [a] -> [(a, Int)]
toFreq x = map (\x -> (head x, length x)) (group $ sort x)

replace :: ReplacementMap -> PairFreqs -> Int -> PairFreqs
replace m freqs 0 = freqs
replace m freqs n = replace m (replacePairs m freqs) (n - 1)

replacePairs :: ReplacementMap -> PairFreqs -> PairFreqs
replacePairs m freqs = mergePairs $ concat $ map newPair freqs
    where newPair pair = replacePair pair (m ! (fst pair))

replacePair :: PairFreq -> String -> PairFreqs
replacePair (pair,freq) c = [(newPair1, freq), (newPair2, freq)]
    where newPair1 = [head pair] ++ c
          newPair2 = c ++ [last pair]

mergePairs :: (Num b, Ord a, Ord b) => [(a, b)]-> [(a, b)]
mergePairs pairs = map sumSnd $ groupBy (\a b -> fst a == fst b) $ sort pairs

sumSnd :: (Num b) => [(a,b)] -> (a,b)
sumSnd xs = (fst $ head xs, sum $ map snd xs)

-- Sliding window size 2, https://twitter.com/GabriellaG439/status/701460899589017600
window2 xs = init $ Data.List.transpose (take 2 (tails xs))

tuple2 [x,y] = (x,y)

