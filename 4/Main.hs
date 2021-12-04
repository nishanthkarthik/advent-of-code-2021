import qualified Data.Text.Lazy as Text
import qualified Control.Monad
import qualified Data.List as List
import qualified Data.Function as Function
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

solve1 :: Input -> [Matrix] -> Int
solve1 input matrices = (input !! bingoIdx) * (sum $ IntSet.toList p1Set)
    where
        matchSets = asMatchSets matrices
        isBingoMatch = (\it -> map (map (flip (IntSet.isSubsetOf) it)) matchSets)
        matches = map isBingoMatch $ map IntSet.fromList $ tail $ List.inits input
        bingoIdx = findFirstOf (id) $ map ((any id) . concat) matches
        boardIdx = findFirstOf (any id) $ matches !! bingoIdx
        p1Set = IntSet.difference (foldl1 (IntSet.union) $ matchSets !! boardIdx) (IntSet.fromList $ take (1 + bingoIdx) input)

solve2 :: Input -> [Matrix] -> Int
solve2 input matrices = (input !! bingoIdx) * (sum $ IntSet.toList p2Set)
    where
        matchSets = asMatchSets matrices
        isBingoMatch = (\it -> map (map (flip (IntSet.isSubsetOf) it)) matchSets)
        matches = map isBingoMatch $ map IntSet.fromList $ tail $ List.inits input
        winStates = map (map $ any id) matches
        allButOneWon = takeWhile (any not) winStates
        bingoIdx = length allButOneWon
        boardIdx = findFirstOf (not) $ last allButOneWon
        p2Set = IntSet.difference (foldl1 (IntSet.union) $ matchSets !! boardIdx) (IntSet.fromList $ take (1 + bingoIdx) input)

main :: IO ()
main = do
    (input, matrices) <- readInput "4/input.txt"
    print (solve1 input matrices)
    print (solve2 input matrices)
    return ()
