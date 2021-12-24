import Data.Maybe
import Data.List
import Data.Array
import Data.Function (on)
import Control.Parallel.Strategies

type State = Array Int [Char]

reference = listArray (0, 10) ["", "", "AA", "", "BB", "", "CC", "", "DD", "", ""]

testData = listArray (0, 10) ["", "", "BA", "", "CD", "", "BC", "", "DA", "", ""] :: State

pathLength :: State -> Int -> Int -> Int
pathLength state from to
    | from `elem` [2,4..8] = abs (to - from) + stackLength (state ! from)
    | otherwise = abs (to - from) + holeLength
    where stackLength arr = if length arr == 2 then 1 else 2
          holeLength = 2 - length (state ! to)

podCost :: Char -> Int
podCost 'A' = 1
podCost 'B' = 10
podCost 'C' = 100
podCost 'D' = 1000

destinationMapping :: Char -> Int
destinationMapping x = (fromJust . lookup x) [('A', 2), ('B', 4), ('C', 6), ('D', 8)]

stackToHole :: State -> Int -> [(Int, Int, Int)]
stackToHole state n
    | null (state ! n) = []
    | length (state ! n) == 1 && head (state ! n) `elem` (reference ! n) = []
    | (state ! n) == (reference ! n) = []
    | otherwise = zip3 (repeat n) allstates (map ((* cost) . pathLength state n) allstates)
    where
        holes = [0..10] \\ [2, 4, 6, 8]
        (lholes, rholes) = partition (< n) holes
        notEmpty = filter (not . null . (state !))
        left = notEmpty lholes
        right = notEmpty rholes
        lstates = if null left then lholes else filter (> maximum left) lholes
        rstates = if null right then rholes else filter (< minimum right) rholes
        allstates = lstates ++ rstates
        cost = podCost (head (state ! n))

holeToStack :: State -> Int -> Maybe (Int, Int, Int)
holeToStack state n
    | null (state ! n) = Nothing
    | not (clearPath && (emptyDest || sameInDest)) = Nothing
    | otherwise = Just (n, dest, multiplier * distance)
    where dest = destinationMapping (head (state ! n))
          clearPath = all (null . (state !)) (path \\ [2,4..8])
          path = if n < dest then [n+1..dest-1] else [dest+1..n-1]
          emptyDest = null (state ! dest)
          sameInDest = (state ! dest) == (state ! n)
          multiplier = podCost . head $ state ! n
          distance = pathLength state n dest

move :: (Int, Int) -> State -> State
move (from, to) state = state // [(from, tail (state ! from)), (to, head (state ! from) : (state ! to))]

step :: State -> Int -> Int
step state score
    | score > 100000 = 100000
    | state == reference = score
    | null allMoves = score + 100000
    | otherwise = minimum (parMap rdeepseq recurse allMoves)
    where stacks = [2,4..8]
          stackMoves = concatMap (stackToHole state) stacks
          holeMoves = mapMaybe (holeToStack state) ([0..10] \\ stacks)
          recurse (fr, to, cost) = step (move (fr, to) state) (score + cost)
          allMoves = holeMoves ++ stackMoves

main :: IO ()
main = do
    let input = listArray (0, 10) ["", "", "DC", "", "CA", "", "DA", "", "BB", "", ""]
        cost = step input 0
    print cost
