import Data.Array (listArray, Array, (!))
import qualified Data.Set as Set

type Algo = Array Int Int
type Image = Set.Set (Int, Int)

readInput :: IO (Algo, Image)
readInput = do
    text <- lines <$> readFile "20/input.txt"
    let algoText = head text
        algo = listArray (0, length algoText - 1) $ map (fromEnum . (== '#')) algoText
    let imageText = drop 2 text
        lx = length imageText
        ly = length $ head imageText
        imageList = zip [(i, j) | i <- [0..lx-1], j <- [0..ly-1]] (concat imageText)
        image = (Set.fromList . map fst . filter ((== '#') . snd)) imageList
    return (algo, image)

bounds :: Image -> ((Int, Int), (Int, Int))
bounds img = ((minX, minY), (maxX, maxY))
    where minX = fst $ Set.findMin img
          maxX = fst $ Set.findMax img
          folder fn ac it = fn ac (snd it)
          minY = Set.foldl (folder min) maxBound img
          maxY = Set.foldl (folder max) minBound img

runKernel :: Algo -> Image -> (Int, Int) -> Int
runKernel algo img it = algo ! toBinary subtiles
    where tiles = map kernelWindow (kernelWindow it)
          safeLookup image it = fromEnum $ Set.member it image
          subtiles = map ((algo !) . toBinary . map (safeLookup img)) tiles
          toBinary = foldl (\ac it -> ac * 2 + it) 0
          kernelWindow (i, j) = [(i + di, j + dj) | di <- [-1..1], dj <- [-1..1]]

enhanceImage :: Algo -> Image -> Image
enhanceImage algo img = (Set.fromList . map fst . filter (toEnum . snd)) applied
    where ((minX, minY), (maxX, maxY)) = bounds img
          flow = 5
          grid = [(x, y) | x <- [minX - flow .. maxX + flow], y <- [minY - flow .. maxY + flow]]
          applied = zip grid $ map (runKernel algo img) grid

main = do
    (algo, img) <- readInput

    let iter = iterate (enhanceImage algo) img
    print $ Set.size (iter !! 1)
    print $ Set.size (iter !! 25)
