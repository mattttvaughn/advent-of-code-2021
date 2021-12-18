import Debug.Trace
import Data.Maybe
import Data.List
import Data.List.Split

main = do
   raw <- readFile "test.txt"
   let byLine = lines raw
   let linesPairedByPipe = map (\x -> tuple2 $ splitOn "|" x) byLine
   let linesToWords = map (\(x,y) -> (words x, words y)) linesPairedByPipe
   -- print linesToWords
   print $ length $ concat $ map snd linesToWords
   print $ countHasUniqueSegs $ concat $ map snd linesToWords
   print $ pt2 linesToWords

tuple2 [x0,x1] = (x0, x1)

countHasUniqueSegs :: [String] -> Int
countHasUniqueSegs xs = sum [1 | x <- xs, (length x) `elem` [2,3,4,7]]

pt2 :: [([String], [String])] -> Int
pt2 r = sum $ map (\(x, y) -> clockVal (deduce $ map sort x) y) r

-- Value of the clock shit for a given line
clockVal :: [String] -> [String] -> Int
clockVal sigs outs | trace ("myfun " ++ show (stringify $ map (\x -> xMachina x sigs) (sort outs))) False = undefined
clockVal sigs outs = stringify $ map (\x -> xMachina x sigs) (sort outs)

stringify xs = read (concatMap show xs) :: Int

xMachina x sigs = fromMaybe 0 $ elemIndex (sort x) $ map sort sigs

-- Input strings should maybe be sorted!
deduce :: [String] -> [String]
deduce x = [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9]
    where p0 = head $ filter (\y -> y /= p6 && y /= p9) $ ofLen 6 x
          p1 = head $ ofLen 2 x
          p2 = head $ (ofLen 5 x) \\ [p3, p5]
          p3 = head $ filter (\y -> p7 == intersect y (p7)) $ ofLen 5 x
          p4 = head $ ofLen 4 x
          p5 = head $ filter (\y -> (length $ p9 \\ y) == 1) (ofLen 5 x)
          p6 = head $ filter (\y -> 3 /= (length $ y \\ p7)) $ ofLen 6 x
          p7 = head $ ofLen 3 x
          p8 = head $ ofLen 7 x
          p9 = head $ filter (\y -> length (y \\ union p7 p4) == 1) $ ofLen 6 x

ofLen n xs = filter (\x -> length x == n) xs

-- 
--   acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
-- | cdfeb fcadb cdfeb cdbaf
--
-- So the first line explains the wirings. So in the case of the ab,
-- presumably that means the "a" and "b" inputs correspond to the the "c"
-- and "f" outputs in some way.
--
-- Following this logic, we get an unordered:
--  "acedgfb" -> 8
--  "ab" -> 1
--  "dab" -> 7
--  "eafb" -> 4
--  "fbcad" -> 3    (because contains "dab" AKA 7)
--  "cdfgeb" -> 6   (because doesn't contain "dab" AKA 7)
--  "cagedb" -> 0   (because doesn't contain "f")
--  "cefabd" -> 9   (because 6 and 0 have been solved for)
--  "gcdfa" -> 2    (because contains "cdfa", which have been solved)
--  "cdfbe" -> 5    (because only one remaining)
--
--  dddd
-- e    a
-- e    a
--  ffff
-- g    b
-- g    b
--  cccc
--
--  Solving outputs:
--     "cdfeb fcadb cdfeb cdbaf"
--
-- "cdfeb" = unjumble "cdfbe" = 5
-- "fcadb" = unjumble "fbcad" = 3
-- "cdfeb" = unjumble "cdfbe" = 5
-- "cdbaf" = unjumble "fbcad" = 3
--
--
-- 
-- 2nd line:
--    be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
--       fdgacbe cefdb cefbgd gcbe
--
-- Following this logic, we get an unordered:
--  "be" -> 1
--  "edb" -> 7
--  "cgeb" -> 4
--  "cfbegad" -> 8
--  "fecdb" -> 3    (because 5er that fully contains 7)
--  "fgaecd" -> 6   (because 6er that don't fully contain 7)
--  "cgebdf" -> 9   (because 6er is (7 + 4) missing 1 letter)
--  "agebfd" -> 0   (because only 6er left)
--  "fdcge" -> 5    (because 5er that's 9 - a letter)
--  "fabcd" -> 2    (leftover)
--
--  dddd
-- cg   be
-- cg   be
--  cgcg
-- x    be
-- x    be
--  xxxx
--
--  Solving outputs:
--      "fdgacbe cefdb cefbgd gcbe"
--
-- unjumble "fdgacbe" = 8
-- unjumble "cefdb" = 3
-- unjumble "cefbgd" = 9
-- unjumble "gcbe" = 4
