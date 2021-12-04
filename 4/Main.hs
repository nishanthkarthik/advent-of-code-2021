import qualified Data.Text.Lazy as Text
import qualified Data.List as List
import qualified Data.Function as Function (on)
import qualified Data.IntSet as IntSet

type Matrix = [[Int]]

type Input = [Int]

readInput :: String -> IO (Input, [Matrix])
readInput f = do
    contents <- readFile f
    let
        text = Text.pack contents
        rows = Text.lines text
        comma = Text.pack ","
        input = map (read . Text.unpack) $ Text.splitOn comma $ head rows
        newlines = Text.pack "\n\n"
        grouByCond = (==) `Function.on` (length . Text.words)
        boards = filter ((== 5) . length) $ List.groupBy grouByCond $ drop 2 rows
        boards' = map (map (map (read . Text.unpack) . Text.words)) $  boards
    return (input, boards')

asMatchSets :: [Matrix] -> [[IntSet.IntSet]]
asMatchSets = map (\it -> (rowSets it) ++ rowSets (List.transpose it))
    where rowSets = map (IntSet.fromList)

findFirstOf :: (a -> Bool) -> [a] -> Int
findFirstOf cond xs = fst . head . filter (\(_, it) -> cond it) $ zip [0..] xs

solution :: Input -> [[IntSet.IntSet]] -> Int -> Int -> Int
solution input matchSets bingoIdx boardIdx = (input !! bingoIdx) * (sum $ IntSet.toList setDiff)
    where
        l = foldl1 (IntSet.union) $ matchSets !! boardIdx
        r = IntSet.fromList $ take (1 + bingoIdx) input
        setDiff = IntSet.difference l r

solve1 :: Input -> [[[Bool]]] -> (Int, Int)
solve1 input matches = (bingoIdx, boardIdx)
    where
        bingoIdx = findFirstOf (id) $ map ((any id) . concat) matches
        boardIdx = findFirstOf (any id) $ matches !! bingoIdx

solve2 :: Input -> [[[Bool]]] -> (Int, Int)
solve2 input matches = (bingoIdx, boardIdx)
    where
        winStates = map (map $ any id) matches
        allButOneWon = takeWhile (any not) winStates
        bingoIdx = length allButOneWon
        boardIdx = findFirstOf (not) $ last allButOneWon

main :: IO ()
main = do
    (input, matrices) <- readInput "4/input.txt"
    let matchSets = asMatchSets matrices
        isBingoMatch = (\it -> map (map (flip (IntSet.isSubsetOf) it)) matchSets)
        matches = map isBingoMatch $ map IntSet.fromList $ tail $ List.inits input
        s1 = uncurry (solution input matchSets) $ solve1 input matches
        s2 = uncurry (solution input matchSets) $ solve2 input matches
    print s1
    print s2
    return ()
