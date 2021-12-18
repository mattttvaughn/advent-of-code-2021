import Data.Maybe
import Data.List
import Data.List.Split

main = do
   raw <- readFile "input.txt"
   let ls = lines raw
   let scored = map (\l -> (scoreLine l, l)) ls 
   let incompleteLines = map snd $ filter (\(s,l) -> s == 0) scored
   print $ sum $ map fst scored 
   print $ median $ map scoreStack $ map makeStack incompleteLines

scoreLine :: String -> Int
scoreLine xs = scoreLine' xs []

scoreLine' :: [Char] -> [Char] -> Int
scoreLine' [] stack = 0
scoreLine' (x:xs) stack
    | isStart x = scoreLine' xs (stack ++ [x]) 
    | isEnd x = if isPair peek x then (scoreLine' xs $ init stack) else score x 
    where peek = last stack

pairs = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
isStart a = any (\(x,_) -> a == x) pairs
isEnd a = any (\(_,y) -> a == y) pairs
isPair x y = any (\(x',y') -> x == x' && y == y') pairs

scores :: [Int]
scores = [3, 57, 1197, 25137]
score endChar = head $ catMaybes $ map (\((_,y),s) -> if endChar == y then Just s else Nothing) (zip pairs scores)

makeStack :: [Char] -> [Char]
makeStack s = makeStack' [] s
makeStack' stack [] = reverse stack
makeStack' stack (x:xs)
    | isStart x = makeStack' (stack ++ [x]) xs
    | isEnd x = makeStack' (init stack) xs

scoreStack :: [Char] -> Int
scoreStack s = scoreStack' s 0
scoreStack' [] acc = acc
scoreStack' (s:ss) acc = scoreStack' ss ((acc * 5) + (stackCharScore s))

stackCharScore c = sum $ map (\((x,_),s) -> if x == c then s else 0) (zip pairs [1,2,3,4])

median ls = (sort ls) !! ((length ls) `div` 2)
