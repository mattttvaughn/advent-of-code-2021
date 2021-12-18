import Data.List
import Data.List.Split
import Data.Maybe
import Data.Function (on)

type Board = [[Int]]
type Game = (Board, Int)

-- Smh there's an off-by-one somewhere, just `- 1`ing for now
calcGameScore b n i = (n !! (i - 1)) * (sum $ getUnused b n (i - 1))

allGames bs ns = (map (\b -> (b, play b ns)) bs)

findBestGame :: [Board] -> [Int] -> Game
findBestGame bs nums = minimumBy (compare `on` snd) (allGames bs nums)

findWorstGame :: [Board] -> [Int] -> Game
findWorstGame bs nums = maximumBy (compare `on` snd) (allGames bs nums)

getUnused b n i = filter (\x -> not $ x `elem` (take (succ i) n)) (concat b)

getMaybe :: Maybe (Int, Int) -> Int
getMaybe x = snd $ fromMaybe (maxBound :: Int, maxBound :: Int) x

-- Given a bingo board and draws, return index of final draw needed
play :: Board -> [Int] -> Int
play b ns = getMaybe $ find (\(n,i) -> hasWon b (take i ns)) (zip ns [0,1..])

hasWon :: Board -> [Int] -> Bool
hasWon lines nums = any (\line -> isLineWinner line nums) (winnableLines)
    where winnableLines = lines ++ transpose lines

isLineWinner :: [Int] -> [Int] -> Bool
isLineWinner line nums = all (\x -> x `elem` nums) line

parseBoards :: [[Char]] -> [Board]
parseBoards x = chunksOf 5 $ map parseNums (filter (\line -> not $ null line) (tail x))

parseNums :: [Char] -> [Int]
parseNums x = map (\i -> read i :: Int) $ split (dropBlanks $ dropDelims $ oneOf ", ") x

main = do
    raw <- readFile "input.txt"
    let a = lines raw
    let bs = parseBoards a
    let ns = parseNums $ head a
    let (winningBoard, finalNumIndex) = findBestGame bs ns
    let (lastBoard, lastNumIndex) = findWorstGame bs ns
    putStrLn $ show $ calcGameScore winningBoard ns finalNumIndex
    putStrLn $ show $ calcGameScore lastBoard ns lastNumIndex
