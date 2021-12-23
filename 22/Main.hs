import Text.Megaparsec (Parsec, parseMaybe, some, optional, choice, parseTest)
import Text.Megaparsec.Char (string, char, lowerChar, space, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (pack, Text)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)

type Parser = Parsec Void Text

type Cube = ((Int, Int), (Int, Int), (Int, Int))

data Step = Step { state :: Bool, cube :: Cube } deriving (Show, Eq, Ord)

parseRange :: Parser (Int, Int)
parseRange = do
    void $ optional $ char ','
    void lowerChar
    void $ char '='
    l <- signed space decimal
    void $ (string . pack) ".."
    r <- signed space decimal
    return (l, r)

parseStep :: Parser Step
parseStep = do
    st <- choice [(string . pack) "on", (string . pack) "off"] :: Parser Text
    void space
    x <- parseRange :: Parser (Int, Int)
    y <- parseRange :: Parser (Int, Int)
    z <- parseRange :: Parser (Int, Int)
    void $ optional newline
    return (Step (st == pack "on") (x, y, z))

readInput :: IO [Step]
readInput = fromJust . parseMaybe (some parseStep) . pack <$> readFile "22/input.txt"

inters3 :: Cube -> Cube -> (Bool, Cube)
inters3 ((x1, x2), (y1, y2), (z1, z2)) ((x3, x4), (y3, y4), (z3, z4)) = (intersects, icube)
    where lx = max x1 x3
          rx = min x2 x4
          ly = max y1 y3
          ry = min y2 y4
          lz = max z1 z3
          rz = min z2 z4
          intersects = lx <= rx && ly <= ry && lz <= rz
          icube = ((lx, rx), (ly, ry), (lz, rz))

volume :: Cube -> Integer
volume ((x1, x2), (y1, y2), (z1, z2)) = toInteger (x2 - x1 + 1) * toInteger (y2 - y1 + 1) * toInteger (z2 - z1 + 1)

solve :: Map.Map Cube Int -> Step -> Map.Map Cube Int
solve mem (Step st cb) = foldl (\ac (k, v) -> Map.insertWith (+) k v ac) mem (zip intersectingUpdateKeys intersectingUpdateVals ++ ownUpdate)
    where intersectingKeys = filter (fst . inters3 cb) (Map.keys mem)
          intersectingUpdateKeys = map (snd . inters3 cb) intersectingKeys
          intersectingUpdateVals = map ((* (-1)) . (mem Map.!)) intersectingKeys
          ownUpdate = [(cb, fromEnum st)]

netVolume :: Map.Map Cube Int -> Integer
netVolume = Map.foldlWithKey (\a k v -> a + toInteger v * volume k) 0

within50 :: Cube -> Bool
within50 (x, y, z) = inLim x && inLim y && inLim z
    where inLim (a, b) = a >= -50 && b <= 50

main :: IO ()
main = do
    i <- readInput
    print $ netVolume $ foldl solve Map.empty $ filter (within50 . cube) i
    print $ netVolume $ foldl solve Map.empty i
