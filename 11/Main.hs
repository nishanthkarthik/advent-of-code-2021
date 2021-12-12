import Data.Char (digitToInt)
import Data.Array
import Data.List
import Data.Maybe

type Idx = (Int, Int)
type Matrix = Array Idx Int

readInput :: String -> IO Matrix
readInput f = do
    f <- readFile f
    let mat = (map (map digitToInt) . lines) f
    return $ listArray ((0, 0), (length mat - 1, (length $ head mat) - 1)) $ concat mat

adj :: Idx -> [Idx]
adj (i, j) = [(i + di, j + dj) | di <- [-1..1], dj <- [-1..1], di /= 0 || dj /= 0]

showMat :: Matrix -> String
showMat m = unlines [unwords [show (m ! (i, j)) | j <- [jl..jr]] | i <- [il..ir]]
    where ((il, jl), (ir, jr)) = bounds m

printMat = putStrLn . showMat

flashStep :: (Matrix, [Idx]) -> (Matrix, [Idx])
flashStep (m, oldNines) = ((stepAll m) // (zip allNines $ repeat 0), allNines)
    where
        allNines = oldNines ++ nines
        nines = filter ((> 9) . ((!) m)) (indices m)
        stepOne m pos = m // [(it, 1 + m ! it) | it <- adj pos, (inRange (bounds m) it) && not (elem it allNines)]
        stepAll m = foldl stepOne m nines

takeSteady :: (Eq b) => (a -> b) -> [a] -> a
takeSteady f (a : b : rest) = if (f a == f b) then a else takeSteady f (b : rest)

step :: (Matrix, [Idx]) -> (Matrix, [Idx])
step (m, i) = (newM, i ++ flashed)
    where stream = iterate flashStep (fmap (+1) m, [])
          (newM, flashed) = takeSteady (length . snd) stream

main :: IO ()
main = do
    i <- readInput "11/input.txt"
    let steplist = (iterate step (i, []))
        n = length (elems i)
    print $ (length . snd) (steplist !! 100)
    print $ length $ takeWhile (/= n) (map (length . (filter (== 0)) . elems . fst) steplist)
