import Data.Array (listArray, Array, (!))
import qualified Data.Set as Set
import Data.Function (on)

type Algo = Array Int Int
type Image = Set.Set (Int, Int)

readInput :: IO (Algo, Image)
readInput = do
    text <- lines <$> readFile "20/input.txt"
    let algoText = head text
        bitOf c = if c == '#' then True else False
        algo = listArray (0, length algoText - 1) $ map (fromEnum . bitOf) algoText
    let imageText = drop 2 text
        lx = length imageText
        ly = length $ head imageText
        imageList = zip [(i, j) | i <- [0..lx-1], j <- [0..ly-1]] (concat imageText)
        image = (Set.fromList . map fst . filter (bitOf . snd)) imageList
    return (algo, image)

kernelWindow :: (Int, Int) -> [(Int, Int)]
kernelWindow (i, j) = [(i + di, j + dj) | di <- [-1..1], dj <- [-1..1]]

showImage :: Image -> String
showImage img = unlines grid
    where ((minX, minY), (maxX, maxY)) = boundsImage img
          pixel i = if i == 1 then '#' else '.'
          grid = [[(pixel . safeLookup img) (x, y) | y <- [minY..maxX]] | x <- [minX..maxX]]

boundsImage :: Image -> ((Int, Int), (Int, Int))
boundsImage img = ((minX, minY), (maxX, maxY))
    where minX = fst $ Set.findMin img
          maxX = fst $ Set.findMax img
          folder fn ac it = fn ac (snd it)
          minY = Set.foldl (folder min) maxBound img
          maxY = Set.foldl (folder max) minBound img

safeLookup :: Image -> (Int, Int) -> Int
safeLookup image it = fromEnum $ Set.member it image

runKernel :: Algo -> Image -> (Int, Int) -> Int
runKernel algo img it = algo ! (toBinary subtiles)
    where tiles = map kernelWindow (kernelWindow it)
          subtiles = map ((algo !) . toBinary . map (safeLookup img)) tiles
          toBinary = foldl (\ac it -> ac * 2 + it) 0

enhanceImage :: Algo -> Image -> Image
enhanceImage algo img = (Set.fromList . map fst . filter (toEnum . snd)) applied
    where ((minX, minY), (maxX, maxY)) = boundsImage img
          flow = 6
          grid = [(x, y) | x <- [minX - flow .. maxX + flow], y <- [minY - flow .. maxY + flow]]
          applied = zip grid $ map (runKernel algo img) grid

main = do
    (algo, img) <- readInput

    let iter = iterate (enhanceImage algo) img
    print $ Set.size (iter !! 1)
    print $ Set.size (iter !! 25)
