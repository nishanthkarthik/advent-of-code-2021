import qualified Data.Map.Strict as Map
import Data.Array (listArray, assocs)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Bifunctor (second)

type Grid = Map.Map (Int, Int) Char
type Input = ((Int, Int), Grid)

readInput :: IO Input
readInput = do
    text <- lines <$> readFile "25/input.txt"
    let x = length text
        y = length $ head text
        arr = listArray ((0, 0), (x - 1, y - 1)) $ concat text
        cucus = filter ((/= '.') . snd) $ assocs arr
    return ((x, y), Map.fromList cucus)

shift :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
shift (dx, dy) dims (x, y) = wrap dims (x + dx, y + dy)
    where wrap (nX, nY) (x, y) = (mod x nX, mod y nY)

move :: Grid -> (Int, Int) -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
move grid dims@(nX, nY) pos@(x, y)
    | grid Map.! pos == '>' = if rPos `Map.notMember` grid then Just (pos, rPos) else Nothing
    | grid Map.! pos == 'v' = if dPos `Map.notMember` grid then Just (pos, dPos) else Nothing
    where rPos = right dims pos
          dPos = down dims pos
          right = shift (0, 1)
          down = shift (1, 0)

step :: Char -> (Int, Int) -> Grid -> Grid
step dir dims grid = updated
    where cucus = mapMaybe (move grid dims) $ Map.keys $ Map.filter (== dir) grid
          removed = foldl (flip Map.delete) grid (map fst cucus)
          delta = Map.fromList $ zip (map snd cucus) (repeat dir)
          updated = Map.union removed delta

showGrid :: (Int, Int) -> Grid -> String
showGrid (nX, nY) grid = unlines [[dot (x, y) | y <- [0..nY-1]] | x <- [0..nX-1]]
    where dot (x, y) = fromMaybe '.' $ grid Map.!? (x, y)

steadyState :: (Eq a) => [a] -> Maybe (Int, a)
steadyState [] = Nothing
steadyState [x] = Just (0, x)
steadyState xs = Just $ Data.Bifunctor.second fst firstEq
    where pairs = zip [1..] (zip xs $ tail xs)
          firstEq = (head . dropWhile (uncurry (/=) . snd)) pairs

main :: IO ()
main = do
    (dims, grid) <- readInput
    let printGrid = putStrLn . showGrid dims
        stepRD = step 'v' dims . step '>' dims
        Just (step1, grid1) = steadyState (iterate stepRD grid)
    print step1
