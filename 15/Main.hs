import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (minimumBy)
import Data.Function (on)
import Data.Ix (inRange)
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Data.Map.Lazy as Map ((!), findMax, fromList, insert, Map, toList, keys)

type Grid = Map.Map (Int, Int) Int

readInput :: String -> IO Grid
readInput f = do
    text <- readFile f
    let nr = (length . lines) text
        nc = (length . head . lines) text
        dims = ((0, 0), (nr - 1, nc - 1))
    return $ Map.fromList $ zip [(i, j) | i <- [0..(nr-1)], j <- [0..(nc-1)]] $ (concatMap (map digitToInt) . lines) text

adjacent :: Grid -> (Int, Int) -> [(Int, Int)]
adjacent grid it@(a, b) = filter validCoord [(a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1)]
    where validCoord = inRange ((0, 0), (fst . findMax) grid)

type PQueue = Heap.MinPrioHeap Int (Int, Int)

dijkstra1' :: Grid -> (Int, Int) -> Grid -> PQueue -> Grid
dijkstra1' graph target dist vertices
    | Heap.null vertices = dist
    | target == u = dist
    | ref /= (dist ! u) = dist
    | otherwise = dijkstra1' graph target nextDist nextQueue
    where (ref, u) = (fromJust . Heap.viewHead) vertices
          nextVert = (fromJust . Heap.viewTail) vertices
          adj = adjacent graph u
          newdist it = min (dist ! it) (graph ! it + dist ! u)
          distDelta = zip adj $ map newdist adj
          updatePQueue q it = if dist ! it > (graph ! it + dist ! u)
                                 then Heap.insert (graph ! it + dist ! u, it) q :: PQueue
                                 else q
          nextQueue = foldl updatePQueue nextVert adj
          nextDist = foldr (uncurry Map.insert) dist distDelta

dijkstra1 :: Grid -> (Int, Int) -> (Int, Int) -> Int
dijkstra1 grid begin end = dijkstra1' grid end dist queue ! end
    where dist = Map.insert begin 0 (maxBound <$ grid)
          queue = Heap.singleton (0, begin) :: PQueue

boundsGrid :: Grid -> (Int, Int)
boundsGrid = fst . Map.findMax

gridify :: Grid -> Int -> Grid
gridify grid n = Map.fromList items
    where
        lim = boundsGrid grid
        (xl, yl) = (fst lim + 1, snd lim + 1)
        transform (i, j) ((x, y), val) = ((x + i * xl, y + j * yl), 1 + mod (val + i + j - 1) 9)
        items = [transform (i, j) item | i <- [0..(n-1)], j <- [0..(n-1)], item <- Map.toList grid]

showMat :: Grid -> String
showMat m = unlines [unwords [show (m ! (i, j)) | j <- [jl..jr]] | i <- [il..ir]]
    where ((il, jl), (ir, jr)) = ((0, 0), boundsGrid m)

printMat = putStrLn . showMat

main :: IO ()
main = do
    i <- readInput "15/input.txt"
    let solve grid = dijkstra1 grid (0, 0) (boundsGrid grid)
    print $ solve i
    print $ solve $ gridify i 5
