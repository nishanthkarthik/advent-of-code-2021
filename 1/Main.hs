import qualified Data.List as List

testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

numberOfIncreases :: (Num a, Ord a) => [a] -> Int
numberOfIncreases x@(_:xs) = length $ filter (> 0) $ zipWith (-) xs x

movingWindow :: Int -> [a] -> [[a]]
movingWindow n = foldr (zipWith (:)) (repeat []) . take n . List.tails

readInput :: IO [Integer]
readInput = do
    text <- readFile "1/input.txt"
    return $ map read $ lines text

main = do
    i <- readInput
    print $ numberOfIncreases i
    print $ numberOfIncreases $ map sum $ movingWindow 3 i
