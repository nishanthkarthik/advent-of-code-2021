import Data.Array
import Data.Char
import Data.Maybe
import Data.List

type Idx = (Int, Int)
type Matrix = Array Idx Int

readInput :: String -> IO Matrix
readInput f = do
    f <- readFile f
    let mat = (map (map digitToInt) . lines) f
    return $ listArray ((0, 0), (length mat - 1, (length $ head mat) - 1)) $ concat mat

safeIdx :: Matrix -> Idx -> Maybe Int
safeIdx m i
    | inRange (bounds m) i = Just (m ! i)
    | otherwise = Nothing

solve1 :: Matrix -> [(Idx, Int)]
solve1 m = filter (cond) $ map (\it -> (it, (m ! it))) $ indices m
    where adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          bounds x = map (safeIdx m) (adjacent x)
          cond (pos, val) = all (fromJust) $ filter isJust $ map (fmap (flip (>) val)) $ bounds pos

bfs :: Matrix -> [Idx] -> Idx -> Int -> [Idx]
bfs m visited cur@(x, y) ref
    | not (inRange (bounds m) cur) = visited
    | (m ! cur) == 9 = visited
    | ref >= (m ! cur) = visited
    | cur `elem` visited = visited
    | otherwise = nub $ concat $ map (\it -> bfs m (cur : visited) it (m ! cur)) neighbors
        where neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

main :: IO ()
main = do
    mat <- readInput "9/input.txt"
    let lowPoints = solve1 mat
    print $ sum $ map ((+ 1) . snd) lowPoints
    print $ (product . (take 3) . sortBy (flip compare) . map length) $ map (\(pos, val) -> bfs mat [] pos (val - 1)) lowPoints
    return ()
