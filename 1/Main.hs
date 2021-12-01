import qualified Data.List as List

testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

numberOfIncreases :: (Num a, Ord a) => [a] -> [a] -> Int
numberOfIncreases l r = length $ filter id $ zipWith (>) r l

readInput :: IO [Integer]
readInput = do
    text <- readFile "1/input.txt"
    return $ map read $ lines text

main = do
    i <- readInput
    print $ numberOfIncreases i $ tail i
    print $ numberOfIncreases i $ drop 3 i
