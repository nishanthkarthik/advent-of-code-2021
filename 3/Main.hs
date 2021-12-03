import Data.List (transpose)

readInput :: IO [[Bool]]
readInput = do
    text <- readFile "3/input.txt"
    return $ map (map $ (== '1')) $ lines text

toBinary :: [Bool] -> Int
toBinary = foldl (\acc it -> fromEnum it + 2 * acc) 0

mostCommonBit :: [Bool] -> Bool
mostCommonBit xs = 2 * (length $ filter id xs) >= length xs

filterCommonExtremaAtIndex :: (Bool -> Bool -> Bool) -> [[Bool]] -> Int -> [[Bool]]
filterCommonExtremaAtIndex comparison xs i = filter ((comparison ref) . (!! i)) xs
    where ref = mostCommonBit $ map (!! i) xs

main :: IO ()
main = do
    input <- readInput
    let commonBits = map mostCommonBit $ transpose input
        gam1 = toBinary commonBits
        eps1 = toBinary $ map (not) commonBits
    print (eps1 * gam1)

    let nbits = length $ head input
        idxs = [0..(nbits - 1)]
        solve2 op = toBinary . head . head $ dropWhile ((> 1) . length) $ scanl (filterCommonExtremaAtIndex op) input idxs
        oxy = solve2 (==)
        co2 = solve2 (/=)
    print (oxy * co2)
